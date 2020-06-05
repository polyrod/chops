{-# Language MultiWayIf #-}
{-# Language BangPatterns #-}
module Chops.Audio (
  loadSample,
  audioThread,
  audioThread',
  MyPortName
)where

import qualified Sound.File.Sndfile               as SF
import qualified Sound.File.Sndfile.Buffer.Vector as BV
import qualified Data.Vector.Storable             as SV

import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Audio                    as Audio

import qualified Sound.JACK.MIDI                     as Midi

import qualified Foreign.C.Error                     as E
import qualified Foreign.C.Types                     as CT

import           System.Environment                  (getProgName)
import           System.Random

import           Data.Array.Storable                 (writeArray)

import           Data.IORef                          (IORef, newIORef,
                                                      readIORef, writeIORef,atomicWriteIORef)

import qualified Data.EventList.Absolute.TimeBody    as EventList
import qualified Data.Map                            as M
import qualified Data.Vector.Generic                 as VG
import qualified Data.Vector.Storable                as V
import qualified Sound.MIDI.Message                  as MM
import qualified Sound.MIDI.Message.Channel          as MMC
import qualified Sound.MIDI.Message.Channel.Voice    as MMV

import           GHC.Float
import           GHC.Int
import           GHC.Word

import           Debug.Trace
import qualified Sound.JACK                          as Jack
import qualified Sound.JACK.Exception                as JackExc
import           System.Mem

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans

import           Control.Monad                       (foldM, forever,
                                                      replicateM)
import           Control.Monad.Random
import           System.IO

import Chops.WSpace 
import Chops.RBuf 
import ConcurrentBuffer
import Control.Concurrent.Chan.Unagi.Bounded

type PlayState = RBuf Double


mainWait ::
     JackExc.ThrowsErrno e
  => Jack.Client
  -> String
  -> PlayState
  -> Sync.ExceptionalT e IO ()
mainWait client name psr =
  Jack.withActivation client $
  Trans.lift $ do
    putStrLn $ "started " ++ name ++ "..."
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    Jack.waitForBreak


type MyPortName = String

--portsFromList :: [MyPortName]

portsFromList [] [] client psr = do
          name <- Trans.lift $ getProgName
          mainWait client name psr

portsFromList [] (p:ps) client psr = do
          JACK.withProcess client (processAudioOut' psr p) $ portsFromList [] ps client psr

portsFromList (x:xs) ps client psr = do
        JACK.withPort client x $ \output -> portsFromList xs (output:ps) client psr

audioThread' :: [MyPortName] -> PlayState -> IO ()
audioThread' pl psr = do
  name <- getProgName
  JACK.handleExceptions $
    JACK.withClientDefault name $ \client ->
      portsFromList pl [] client psr


audioThread :: PlayState -> IO ()
audioThread psr = do
  name <- getProgName
  JACK.handleExceptions $
    JACK.withClientDefault name $ \client ->
      JACK.withPort client "input" $ \input ->
        JACK.withPort client "output" $ \output ->
          JACK.withProcess client (processAudioOut psr input output) $
          mainWait client name psr

processAudioOut' ::
     PlayState
  -> Audio.Port JACK.Output
  -> JACK.NFrames
  -> Sync.ExceptionalT E.Errno IO ()
processAudioOut' psr output nframes@(JACK.NFrames nframesInt) = do
  Trans.lift $ do
    outArr <- Audio.getBufferArray output nframes
    case JACK.nframesIndices nframes of
      [] -> return ()
      idxs ->
        mapM_
          (\i@(JACK.NFrames ii) -> do
             f <- nextFrame psr i
             writeArray outArr i (CT.CFloat $ double2Float f))
          idxs


processAudioOut ::
     PlayState
  -> Midi.Port JACK.Input
  -> Audio.Port JACK.Output
  -> JACK.NFrames
  -> Sync.ExceptionalT E.Errno IO ()
processAudioOut psr input output nframes@(JACK.NFrames nframesInt) = do
  mel <- EventList.toPairList <$> Midi.readEventsFromPort input nframes
  Trans.lift $ do
  {-
    mapM_
      (\(t, b) -> do
         ps <- readIORef psr
         case b of
           MM.System _ -> return ()
           MM.Channel ct -> do
             let c = MMC.fromChannel $ MMC.messageChannel ct
             unless (c /= 1) $ 
               case MMC.messageBody ct of
                      MMC.Mode _ -> return ()
                      MMC.Voice vt -> if | otherwise -> return ())
                           {-
                           | isPresetCntrl vt ->
                             do let (MMV.Control _ cv) = vt
                                setPreset psr cv
                           | MMV.isNoteOn vt ->
                             do let (MMV.NoteOn p v') = vt
                                let v = MMV.fromVelocity v'
                                let (JACK.NFrames nf) = t
                           | MMV.isNoteOff vt -> -}
      mel -}

    outArr <- Audio.getBufferArray output nframes
    case JACK.nframesIndices nframes of
      [] -> return ()
      idxs ->
        mapM_
          (\i@(JACK.NFrames ii) -> do
             f <- nextFrame psr i
             writeArray outArr i (CT.CFloat $ double2Float f))
          idxs

nextFrame :: PlayState -> Jack.NFrames -> IO Double
nextFrame psr i = do
  h <- pullRBuf psr
  return h


toOneChannel []       = []
toOneChannel [s]      = []
toOneChannel (l:_:ss) = l : toOneChannel ss

loadSample :: String -> IO (SV.Vector Double)
loadSample fn = do
  (info, Just !x) <- SF.readFile fn
  print info
  return $
    SV.map (* 0.5) $
    if SF.channels info == 2
      then SV.fromList $ toOneChannel $ SV.toList $ BV.fromBuffer x
      else BV.fromBuffer x


