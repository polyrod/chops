{-# Language MultiWayIf #-}
module Chops.Audio (
  loadSample,
  audioThread
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
                                                      readIORef, writeIORef)

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

type PlayState = M.Map String WSpace


mainWait ::
     JackExc.ThrowsErrno e
  => Jack.Client
  -> String
  -> IORef PlayState
  -> Sync.ExceptionalT e IO ()
mainWait client name psr =
  Jack.withActivation client $
  Trans.lift $ do
    putStrLn $ "started " ++ name ++ "..."
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    Jack.waitForBreak

audioThread :: IORef PlayState -> IO ()
audioThread psr = do
  name <- getProgName
  JACK.handleExceptions $
    JACK.withClientDefault name $ \client ->
      JACK.withPort client "input" $ \input ->
        JACK.withPort client "output" $ \output ->
          JACK.withProcess client (processAudioOut psr input output) $
          mainWait client name psr

processAudioOut ::
     IORef PlayState
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

nextFrame :: IORef PlayState -> Jack.NFrames -> IO Double
nextFrame psr i = do
  wsps <- readIORef psr
  let (val,wsps') = M.mapAccum getFrame 0 wsps
  writeIORef psr  wsps'
  return val 


getFrame :: Double -> WSpace -> (Double,WSpace)
getFrame a ws = if 
                | _play ws == Stop -> (a,ws)
                | _len ws == 0 -> (a,ws)
                | _play ws == Fwd -> let val = ws2sample ws
                                      in (a+val,ws { _tick = _tick ws + 1 })
                | _play ws == Bwd -> let val = ws2sample ws
                                      in (a+val,ws { _tick = let t = _tick ws - 1
                                                              in if t > 0
                                                                  then t
                                                                  else floor $_vlen ws * fromIntegral (_len ws)})






ws2sample ws = let avlen = floor $ _vlen ws * fromIntegral (_len ws)
                   ridx  = fromIntegral (_tick ws  `mod` avlen ) / fromIntegral avlen
                   aidx = fromIntegral (_len ws) * ridx
                   faidx = floor aidx
                   caidx = (faidx + 1) `mod` _len ws
                   frcaidx = aidx - fromIntegral faidx
                   fval = _data ws V.! fromIntegral faidx
                   cval = _data ws V.! fromIntegral caidx
                in fval + ((cval-fval) * frcaidx)




toOneChannel []       = []
toOneChannel [s]      = []
toOneChannel (l:_:ss) = l : toOneChannel ss

loadSample :: String -> IO (SV.Vector Double)
loadSample fn = do
  (info, Just x) <- SF.readFile fn
  print info
  return $
    SV.map (* 0.5) $
    if SF.channels info == 2
      then SV.fromList $ toOneChannel $ SV.toList $ BV.fromBuffer x
      else BV.fromBuffer x


