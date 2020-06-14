module Main where



import Chops

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  loadNRun $ args !! 0

