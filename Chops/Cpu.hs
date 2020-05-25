{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleContexts #-}
{-# Language TypeApplications #-}
{-# Language MultiWayIf #-}
{-# Language OverloadedStrings #-}
module Chops.Cpu where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Extra
import Control.Monad.RWS
import Control.Concurrent

import Data.List
import Data.Ord
import Data.Function

import Data.IORef

import System.Clock

import Formatting

import Chops.AST
import Chops.Time
import Chops.Parser
import Chops.Audio
import Chops.WSpace

import qualified Data.Vector.Storable             as SV


import Debug.Trace

type Program = [Stmt]


defaultWSpace = WSpace [] SV.empty 0 1.5 0 Fwd M.empty 

ldWspace :: String -> Computation WSpace
ldWspace wsn = do
  vm <- get
  wsps <- liftIO $ readIORef $ _wspaces vm
  case M.lookup wsn wsps of
    Nothing -> do let nws = defaultWSpace 
                  liftIO $ modifyIORef' (_wspaces vm) (M.insert wsn nws) 
                  modify(\vm -> vm {_cwspace = Just wsn})
                  pure nws
    Just ws' -> do
                  modify(\vm -> vm {_cwspace = Just wsn})
                  pure ws'
    
modifyWspace :: (WSpace -> WSpace) -> Computation ()
modifyWspace f = 
  do
      vm <- get
      wsps <- liftIO $ readIORef $ _wspaces vm
      case _cwspace vm of
        Nothing -> error "Cant load into non-existing Workspace"
        Just wsn -> liftIO $ modifyIORef' (_wspaces vm) 
                              (\wsps -> case M.lookup wsn wsps of
                                          Nothing -> wsps
                                          Just ws -> M.insert wsn (f ws) wsps) 


data SyncFlag = SyncWait TimeSpec
              | NoSync
             deriving (Eq,Ord,Show)


data VM = VM { _ip      :: Integer
             , _env     :: Env
             , _prg     :: Program
             , _wspaces :: IORef (M.Map String WSpace)
             , _cwspace :: Maybe String
             , _sflag   :: SyncFlag
             , _tspec   :: MTSpec
             , _halt    :: Bool
             , _isJmp   :: Bool
             }
     --        deriving Show


newtype Cpu r w s a =
  Cpu
    { getCpu :: RWST r w s IO a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader r
           , MonadWriter w
           , MonadState s
           , MonadIO
           )

runCpu :: r -> s -> Cpu r w s a -> IO (a,s,w)
runCpu r vm comp = runRWST (getCpu comp) r vm 




type Computation a = Cpu () [String] VM a

stepIP :: Computation ()
stepIP = modify (\cpu -> if _isJmp cpu then cpu { _isJmp = False } else cpu { _ip = _ip cpu +1 }) -- >> tellIP

jmpIP :: Addr -> Computation ()
jmpIP a = modify (\cpu -> cpu { _ip = a , _isJmp = True })

fetchIns :: Computation Stmt
fetchIns = do
  ip <- _ip <$> get
  (!! fromIntegral ip) . _prg <$> get

setSFlag :: SyncFlag -> Computation ()
setSFlag f = modify (\cpu -> cpu { _sflag = f })

tickSleep :: Computation ()
tickSleep = do
  ts <- _tspec <$> get
  let t =  fromIntegral (toNanoSecs $ valToCTS ts (MTVal 0 0 1)) `div` 2000 
  liftIO $ threadDelay t
  
operateVM :: Computation ()
operateVM = do
  hlt <- _halt <$> get
  unless hlt $
     do
  --    tickSleep
      sf <- _sflag <$> get
      case sf of
        SyncWait t1 -> do 
                    liftIO $ do 
                        delta <- loopM  (\i -> do  
                                                    threadDelay 10000
                                                    t  <- getTime Realtime
                                                    if t < t1
                                                      then return $ Left (i+1)
                                                      else return $ Right $ (i,(t - t1)) ) 0
                        print delta                                

                    setSFlag NoSync
                    stepIP
                    fetchDecodeExecuteLoop
        NoSync -> do
          stepIP
          fetchDecodeExecuteLoop

{-
operateVM :: Computation ()
operateVM = do
  hlt <- _halt <$> get
  unless hlt $
     do
  --    tickSleep
      sq <- _syncq <$> get
      sf <- _sflag <$> get
      t  <- liftIO $ getTime Realtime
      let cq@(sq',sqtail) = partition (\(a,_,_) -> a <= t) $ nub sq
      if null sq'
        then do
              setSyncQ sqtail
              if sf == SyncWait 
                then operateVM
                else do
                  stepIP
                  fetchDecodeExecuteLoop
        else do
          let sq'' = sortBy (compare `on` (\(_,a,_) -> a)) sq'
              e1@(t1,_,addr) = head sq''
              newq = tail sq'' <> sqtail
              e2@(t2,f,_) = head newq
          if
           | null newq -> setSFlag NoSync
           | (f == SyncOn) -> setSFlag SyncOn
           | (f == SyncINT) -> setSFlag SyncINT
           | True -> error "Shouldnt happen :("

          setSyncQ newq 
          jmpIP addr
          fetchDecodeExecuteLoop
-}

tellVM = do
  vm <- get
  s <- displayVM
  tell [s]
          
tellIP = do
  vm <- get
  s <- displayIP
  tell [s]


fetchDecodeExecuteLoop :: Computation ()
fetchDecodeExecuteLoop = do
  ins <- fetchIns
  exec ins
  operateVM


exec :: Stmt -> Computation ()
exec ins = do
  case ins of
        BGN s -> tell ["\nRunning Program '" ++ s ++ "'\n"]
        JMP a -> jmpIP a 
        JDNZ a addr -> do
                    env <- _env <$> get
                    let x  = envLookup env a :: Integer
                        x' = x - 1
                        env' = M.insert a (injt  x') env
                    modify (\vm -> vm { _env = env' } )
                    if x' /= 0
                      then jmpIP addr
                      else pure ()
        BPM b -> do
                  ts <- _tspec <$> get
                  ts' <- liftIO $ timeSpecIO b (_sig ts) (_res ts)
                  modify (\vm -> vm { _tspec = ts' } )

        SIG b c -> do
                  ts <- _tspec <$> get
                  ts' <- liftIO $ timeSpecIO (_bpm ts) (b,c) (_res ts)
                  modify (\vm -> vm { _tspec = ts' } )

        SRST -> do
                  ts <- _tspec <$> get
                  ts' <- liftIO $ timeSpecIO (_bpm ts) (_sig ts) (_res ts)
                  modify (\vm -> vm { _tspec = ts' } )

        FIT s -> do
                  ts <- _tspec <$> get
                  env <- _env <$> get
                  modifyWspace (\ws-> let sspb = 60.0 * 44100.0 / _bpm ts
                                          sspbar = sspb * fromIntegral (fst $ _sig ts)
                                          fspbar = fromIntegral $ _len ws
                                          vlen' = (traceShow sspbar sspbar) / fspbar 
                                          ws' = ws { _vlen = (eval env s) * vlen' }
                                       in traceShow (floor $ (fromIntegral $ _len ws) * _vlen ws') ws')   



        WAIT (brs,bts,pls) -> do 
                                ts <- _tspec <$> get
                                let dt = valToCTS ts (MTVal (fromIntegral brs) 
                                                            (fromIntegral bts) 
                                                            (fromIntegral pls)
                                                     ) 
                                t0  <- liftIO $ getTime Realtime
                                let tt = t0 + dt 
                                modify (\vm -> vm { _sflag = SyncWait tt } )

        WAITT (brs,bts,pls) -> do 
                                ts <- _tspec <$> get
                                let t1 = valToCTS ts (MTVal (fromIntegral brs) 
                                                            (fromIntegral bts) 
                                                            (fromIntegral pls)
                                                     ) 
                                let tt = _origin ts + t1 
                                modify (\vm -> vm { _sflag = SyncWait tt } )


        SET s a -> do 
                    env <- _env <$> get
                    let env' = case M.lookup s env of
                                Nothing -> M.insert s (injt $ eval env a) env
                                Just _  -> M.insert s (injt $ eval env a) env
                    modify (\vm -> vm { _env = env' } )

        MRK s a -> do 
                    env <- _env <$> get
                    modifyWspace (\ws ->  ws { _marks = M.insert s (eval env a) (_marks ws)})

        SEL ws -> do 
                    ldWspace ws 
                    pure ()

        LD f -> do 
                  vm <- get
                  let fn = eval (_env vm) f
                  ad <- liftIO $ loadSample fn
                  modifyWspace (\ws-> ws { _data = ad , _len = fromIntegral $ SV.length ad })

        PLAYF m -> modifyWspace (\ws->let mv = M.lookup m (_marks ws)
                                     in case mv of 
                                          Nothing -> ws { _play = Fwd }
                                          Just v -> let avpos = floor $ v * fromIntegral (_len ws) * _vlen ws 
                                                     in ws { _tick = avpos , _play = Fwd }
                                )

        PLAYB m -> modifyWspace (\ws->let mv = M.lookup m (_marks ws)
                                     in case mv of 
                                          Nothing -> ws { _play = Bwd }
                                          Just v -> let avpos = floor $ v * fromIntegral (_len ws) * _vlen ws 
                                                     in ws { _tick = avpos , _play = Bwd }
                                )
        SEEK v -> do  
                      vm <- get
                      let f = eval (_env vm) v
                      modifyWspace (\ws->let avpos = floor $ f * fromIntegral (_len ws) * _vlen ws 
                                          in ws { _tick = avpos}
                                )


        STOP -> modifyWspace (\ws-> ws { _play = Stop})
        HLT -> modify (\vm -> vm { _halt = True } )
                  
        _ -> return ()
        
  vm <- get
 -- tellVM
  when (_halt vm) $ tell ["\n\nHalting .\n"]

reset :: Program -> IO VM
reset prg = do
  s <- timeSpecIO 180 (4,4) 96
  wsps <- newIORef M.empty
  forkIO $ audioThread wsps
  pure $ VM 0 M.empty prg wsps Nothing  NoSync s False True

runComputation :: Program -> IO String 
runComputation prg = do
  p <- reset prg
  (res,s,w) <- runCpu () p operateVM
  return $  concat w

loadNRun = do
  f <- readFile "s1.prg"
  case prs f of
    (Left x) -> print x
    (Right s) -> do 
                  mapM_ (\(ni,i) -> do
                          let (mnc:ps) = words $ show i 
                          putStr $ formatToString ("\n" % left 4 '0' % "| ") ni 
                          putStr $ formatToString ( right 7 ' ') mnc 
                          mapM_ (putStr . formatToString string) ps) s 
                  putStrLn "\n--------------------------------------\n"
                  r <- runComputation $ map snd s
                  putStrLn r

displayIP :: Computation String
displayIP = do
            vm <- get
            let t0 = _origin $ _tspec vm
            tnow <- liftIO $ getTime Realtime
            let dt = toNanoSecs $ diffTimeSpec tnow t0
                (nbars,rbars) = dt `divMod` toNanoSecs (valToCTS (_tspec vm) (MTVal 1 0 0))
                (nbts,rbts) = rbars `divMod` toNanoSecs (valToCTS (_tspec vm) (MTVal 0 1 0))
                (npls,rpls) = rbts `divMod` toNanoSecs (valToCTS (_tspec vm) (MTVal 0 0 1))
                time = formatToString ("[" % left 4 '0'  % ":" 
                                           % left 2 '0'  % ":" 
                                           % left 3 '0' %  "]") nbars nbts npls 

            return $ "\n\n" ++ time ++ " IP : " ++ formatToString (left 8 '0') (show $ _ip vm)


displayVM :: Computation String
displayVM = do
            vm <- get
            return $  "\n => HLT : " ++ show (_halt vm) 
                   ++ " ENV : " ++ show (_env vm) 
                   ++ " CWSP : " ++ show (_cwspace vm) 
                   ++ " SFLAG : " ++ show (_sflag vm)


{-

          bgn     "foo"
          para    fn,"audio"
          sel     au
          ld      fn
          bpm     120
          sig     11,4
          mrk     m1,1 / 8
          madj    m1,0.001
          set     cx,11
          goto    m1
          playf
loop:
          wait    qb
          goto    m1
          jdnz    cx,loop
          onbeat  m1
          stop
          end


-}
