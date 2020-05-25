{-# Language TypeApplications #-}
 module Chops.Time where

import System.Clock

type BPM = Float
type SIG = (Int,Int)
type Pulse = Int


data MTSpec = MTSpec {_bpm :: BPM
                     ,_sig :: SIG
                     ,_res :: Pulse
                     ,_origin :: TimeSpec
                     }
                     deriving Show

data MTVal = MTVal  { _bar    :: Int
                    , _beat   :: Int
                    , _pulse  :: Int
                    }
                   

valToCTS :: MTSpec -> MTVal -> TimeSpec
valToCTS (MTSpec bpm (bpbar,counton) res org) (MTVal bar beat pulse) = 
  let durBeat = 10000000000 * 60 `div` floor  (10 * bpm)  
      durBars = bar * bpbar * durBeat
      durBeats = beat * durBeat
      durPulses = durBeat *  floor (1000000000 *  fromIntegral pulse / fromIntegral res) `div` 1000000000  
      durVal = durBars + durBeats + durPulses
  in fromNanoSecs $ fromIntegral durVal

timeSpecIO :: BPM -> SIG -> Pulse -> IO MTSpec
timeSpecIO bpm sig resol = 
  do
    cputime <- getTime Realtime
    return $ MTSpec bpm sig resol cputime
  
  

