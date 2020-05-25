{-# Language TypeApplications #-}
 module Chops.Time where


type BPM = Float
type SIG = (Int,Int)
type Pulse = Int
type SamplesPerSecond = Integer


data MTSpec = MTSpec {_bpm :: BPM
                     ,_sig :: SIG
                     ,_res :: Pulse
                     ,_sps :: SamplesPerSecond
                     }
                     deriving Show

data MTVal = MTVal  { _bar    :: Int
                    , _beat   :: Int
                    , _pulse  :: Int
                    }
                     deriving Show
                   

mtValToSamCount :: MTSpec -> MTVal -> SamplesPerSecond
mtValToSamCount (MTSpec bpm (bpbar,counton) res sps) (MTVal bar beat pulse) = 
  let durBeat = 60.0 / bpm  
      durBars = fromIntegral bar * fromIntegral bpbar * durBeat
      durBeats = fromIntegral beat * durBeat
      durPulses = durBeat *  (fromIntegral pulse / fromIntegral res)   
      durVal = durBars + durBeats + durPulses
  in floor $ durVal * fromIntegral sps

timeSpec :: BPM -> SIG -> Pulse -> SamplesPerSecond -> MTSpec
timeSpec bpm sig resol sps = MTSpec bpm sig resol sps
  
  

