{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleContexts #-}
{-# Language TypeApplications #-}
{-# Language MultiWayIf #-}
{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}
module Chops.Cpu where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Extra
import Control.Monad.RWS
import Control.Concurrent (threadDelay,forkIO)

import Data.List
import Data.Ord
import Data.Tuple (swap)
import Data.Function

import Data.IORef


import Formatting

import Chops.AST
import Chops.Time
import Chops.Parser
import Chops.Audio
import Chops.WSpace
import Chops.RBuf

import qualified Data.Vector.Storable             as SV


import Debug.Trace
import ConcurrentBuffer

import System.Mem
import Control.DeepSeq

buffsize = 2048  

type Program = [Stmt]


defaultWSpace vm = pure $ WSpace [] SV.empty 0 1.0 0 Fwd M.empty (_dbname vm)

ldWspace :: String -> Computation WSpace
ldWspace wsn = do
  vm <- get
  let wsps = _wspaces vm
  case M.lookup wsn wsps of
    Nothing -> do nws <- defaultWSpace vm
                  modify(\vm -> vm {_cwspace = Just wsn,_wspaces = M.insert wsn nws (_wspaces vm)})
                  pure nws
    Just ws' -> do
                  modify(\vm -> vm {_cwspace = Just wsn})
                  pure ws'
    
modifyWspace :: (WSpace -> WSpace) -> Computation ()
modifyWspace f = 
  do
      vm <- get
      case _cwspace vm of
        Nothing -> error "Cant load into non-existing Workspace"
        Just wsn -> modify (\vm -> let wsps = _wspaces vm 
                                    in vm { _wspaces = case M.lookup wsn wsps of
                                                          Nothing -> wsps
                                                          Just ws -> M.insert wsn (f ws) wsps }) 

type Tick = Integer

data SyncFlag = SyncWait Tick
              | NoSync
             deriving (Eq,Ord,Show)


data VM = VM { _ip        :: Integer
             , _env       :: !Env
             , _prg       :: Program
             , _wspaces   :: !(M.Map String WSpace)
             , _cwspace   :: Maybe String
             , _sflag     :: !SyncFlag
             , _tspec     :: MTSpec
             , _halt      :: Bool
             , _isJmp     :: Bool
             , _gtick     :: !Tick
             , _dbname    :: String
             , _buffmap   :: !(M.Map String (RBuf Double))
             }
     --        deriving Show


newtype Cpu r w s a =
  Cpu
    { getCpu :: (RWST r w s IO a)
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
stepIP = modify (\cpu -> if _isJmp cpu then cpu { _isJmp = False } 
                                       else cpu { _ip = _ip cpu +1 }) -- >> tellIP

jmpIP :: Addr -> Computation ()
jmpIP a = modify (\cpu -> 
            cpu { _ip = a , _isJmp = True })

fetchIns :: Computation Stmt
fetchIns = do
  ip <- _ip <$> get
  (!! fromIntegral ip) . _prg <$> get

setSFlag :: SyncFlag -> Computation ()
setSFlag f = modify (\cpu -> cpu { _sflag = f })

operateVM :: Computation ()
operateVM = do
  hlt <- _halt <$> get
  unless hlt $
     do
      sf <- _sflag <$> get
      case sf of
        NoSync -> do
          stepIP
          fetchDecodeExecuteLoop
        SyncWait t1 ->do
              tnow <- _gtick <$> get
              if t1 > tnow
                then do
                      bufmap <- _buffmap <$> get
                      wsps <- _wspaces <$> get
                      --liftIO . print . map fst $ M.toList wsps
                      --liftIO . print . map fst $ M.toList bufmap
                      let !(!vmap,!wsps') = M.mapAccum getFrame (M.empty) wsps
                      _ <- M.traverseWithKey (\name val -> liftIO $ pushRBuf (bufmap M.! name) val) vmap
                      modify (\vm -> vm { _gtick = _gtick vm + 1 ,_wspaces = wsps'})
                      operateVM
                else do
                      liftIO $ putStrLn $ "GTicK: " ++ (show tnow) 
                      setSFlag NoSync
                      --liftIO $ performMinorGC
                      liftIO performGC
                      stepIP
                      fetchDecodeExecuteLoop


getFrame :: M.Map String Double -> WSpace -> (M.Map String Double,WSpace)
getFrame a ws = 
  if 
  | _play ws == Stop -> (a,ws)
  | _len ws == 0 -> (a,ws)

  | _play ws == Fwd -> 
      let !val = ws2sample ws
       in (M.insertWith (+) (_obufname ws) val a
          ,ws { _tick = _tick ws + 1 })

  | _play ws == Bwd -> 
      let !val = ws2sample ws
       in (M.insertWith (+) (_obufname ws) val a
          ,ws { _tick = let t = _tick ws - 1
                         in if t > 0
                             then t
                             else floor $ 
                               _vlen ws * fromIntegral (_len ws)})





ws2sample ws = let avlen = floor $ _vlen ws * fromIntegral (_len ws)
                   ridx  = fromIntegral (_tick ws  `mod` avlen ) / fromIntegral avlen
                   aidx = fromIntegral (_len ws) * ridx
                   faidx = floor aidx
                   caidx = (faidx + 1) `mod` _len ws
                   !frcaidx = aidx - fromIntegral faidx
                   !fval = _data ws SV.! fromIntegral faidx
                   !cval = _data ws SV.! fromIntegral caidx
                in fval + ((cval-fval) * frcaidx)



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

        JDGZ a addr -> do
                    env <- _env <$> get
                    let x  = envLookup env a :: Integer
                        x' = x - 1
                        env' = M.insert a (injt  x') env
                    modify (\vm -> vm { _env = env' } )
                    if x' > 0
                      then jmpIP addr
                      else pure ()

        BPM b -> do
                  ts <- _tspec <$> get
                  let ts' = timeSpec b (_sig ts) (_res ts) (_sps ts)
                  modify (\vm -> vm { _tspec = ts' } )

        SIG b c -> do
                  ts <- _tspec <$> get
                  let ts' = timeSpec (_bpm ts) (b,c) (_res ts) (_sps ts)
                  modify (\vm -> vm { _tspec = ts' } )

        SRST -> do
                  ts <- _tspec <$> get
                  let ts' = timeSpec (_bpm ts) (_sig ts) (_res ts) (_sps ts)
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
                                let dt = mtValToSamCount ts (MTVal (fromIntegral brs) 
                                                            (fromIntegral bts) 
                                                            (fromIntegral pls)
                                                     ) 
                                t0 <- _gtick <$> get
                                let tt = t0 + dt 
                                modify (\vm -> vm { _sflag = SyncWait tt } )

        WAITT (brs,bts,pls) -> do 
                                ts <- _tspec <$> get
                                let t1 = mtValToSamCount ts (MTVal (fromIntegral brs) 
                                                            (fromIntegral bts) 
                                                            (fromIntegral pls)
                                                     ) 
                                let tt = t1 
                                modify (\vm -> vm { _sflag = SyncWait tt } )


        SET s a -> do 
                    env <- _env <$> get
                    let env' = M.insert s (injt $ eval env a) env
                    modify (\vm -> vm { _env = env' } )

        MRK s a -> do 
                    env <- _env <$> get
                    modifyWspace (\ws ->  ws { _marks = M.insert s (eval env a) (_marks ws)})

        SOP pn -> do 
                    bufmap <- _buffmap <$> get
                    modifyWspace (\ws -> case M.lookup pn bufmap of
                                            Just _ -> ws { _obufname = pn}
                                            Nothing -> ws)

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

runComputation :: [String] -> Program -> IO String 
runComputation ps prg = do
  let s = timeSpec 180 (4,4) 96 44100
  bl <- if null ps
          then do
                rb <- newRBuf buffsize
                pure [("output",rb)]
          else do
                pbs <- mapM (\pn -> do
                      b <- newRBuf buffsize
                      return (pn,b)) $ nub ps
                return pbs
  let defb = fst $ head bl               
                
  let p = VM 0 M.empty prg M.empty Nothing  NoSync s False True 0 defb (M.fromList bl) 
  forkIO $ audioThread' bl 
  runCpu () p operateVM
  return [] 

loadNRun fn = do
  f <- readFile fn
  case prs fn f of
    (Left x) -> print x
    (Right (ps,s)) -> do 
                  mapM_ (\(ni,i) -> do
                          let (mnc:ps) = words $ show i 
                          putStr $ formatToString ("\n" % left 4 '0' % "| ") ni 
                          putStr $ formatToString ( right 7 ' ') mnc 
                          mapM_ (putStr . formatToString string) ps) s 
                  putStrLn "\n--------------------------------------\n"
                  mapM_ (\p -> putStrLn $ "Output Port : " ++ p ++ "\n") ps
                  putStrLn "--------------------------------------\n"
                  r <- runComputation ps $ map snd s
                  putStrLn r

displayIP :: Computation String
displayIP = do
            vm <- get
            let t0 = 0 
                tnow = _gtick vm 
            let dt = tnow - t0
                (nbars,rbars) = dt `divMod` (mtValToSamCount (_tspec vm) (MTVal 1 0 0))
                (nbts,rbts) = rbars `divMod`(mtValToSamCount (_tspec vm) (MTVal 0 1 0))
                (npls,rpls) = rbts `divMod` (mtValToSamCount (_tspec vm) (MTVal 0 0 1))
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
