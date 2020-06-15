module Chops(
  loadNRun
)where


import Chops.Cpu
import Chops.Audio
import Chops.AST
import Chops.Parser
import Chops.Time
import Chops.WSpace
import Chops.RBuf

import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.Async
import Data.List

buffsize = 1024  

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
  race_ (audioThread' bl) 
        (do 
          threadDelay 2000000
          runCpu () p operateVM
          return ())

  return [] 

loadNRun fn = do
  f <- readFile fn
  case prs fn f of
    (Left x) -> print x
    (Right (ps,s)) -> do 
                  mapM_ (displayIns) s 
                  putStrLn "\n--------------------------------------\n"
                  mapM_ (\p -> putStrLn $ "Output Port : " ++ p ++ "\n") ps
                  putStrLn "--------------------------------------\n"
                  r <- runComputation ps $ s
                  putStrLn r


