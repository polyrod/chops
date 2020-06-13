{-# LANGUAGE DeriveAnyClass #-}
module Chops.RBuf(
  RBuf,
  newRBuf,
  pushRBuf,
  pullRBuf,
)where

import qualified Data.Vector.Storable.Mutable as MSV
import Data.IORef
import Control.Monad.Extra
import Control.Concurrent
import System.Mem

newtype RBuf a = RBuf (IORef (RingBuff a))

data RingBuff a = RingBuff {
    _buff  :: !(MSV.IOVector a),
    _size  :: !Int,
    _rdidx :: !Int,
    _wtidx :: !Int
  }

newRBuf :: MSV.Storable a => Int -> IO (RBuf a)
newRBuf size = do
  rbuf <- MSV.new size
  rb <- newIORef $ RingBuff rbuf size 0 0
  return $ RBuf rb


pushRBuf :: (MSV.Storable a) => RBuf a -> a -> IO ()
pushRBuf b@(RBuf rb) a = do
  rbuf <- readIORef rb
  if ((_wtidx rbuf + 1) `mod` (_size rbuf)) == (_rdidx rbuf)
    then do 
      threadDelay 100
      pushRBuf b a
    else do
      MSV.write (_buff rbuf) (_wtidx rbuf) a
      atomicModifyIORef' rb (\rbuf -> (rbuf { _wtidx = (_wtidx rbuf + 1) `mod` (_size rbuf) },())) 
      

pullRBuf :: (MSV.Storable a) => RBuf a -> IO a
pullRBuf (RBuf rb) = do
  rbuf <- readIORef rb
  if ((_rdidx rbuf + 1) `mod` (_size rbuf)) == (_wtidx rbuf)
    then do
          a <- MSV.read (_buff rbuf) (_rdidx rbuf) 
          return a
    else do
          a <- MSV.read (_buff rbuf) (_rdidx rbuf) 
          atomicModifyIORef' rb (\rbuf -> (rbuf { _rdidx = (_rdidx rbuf + 1) `mod` (_size rbuf) },())) 
          return a
  

testRB rb = do
  let t1 i = do 
               threadDelay 300000
               pushRBuf rb i
               threadDelay 100000
               putStrLn $ "(" ++ (show i) ++ ")"
               t1 (i+1)
  let t2 = do 
            threadDelay 1000000
            v <- pullRBuf rb
            putStrLn $ "V: " ++ show v
            t2
  forkIO $ t1 0
  forkIO t2
