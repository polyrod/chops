module Chops.WSpace where

import qualified Data.Vector.Storable             as SV
import qualified Data.Map                         as M
import Control.DeepSeq

data PlayMode = Stop | Fwd | Bwd
                  deriving (Show,Eq)
instance NFData PlayMode  where
  rnf _ = ()

type SData = SV.Vector Double

data WSpace =  WSpace { _fname :: String
                      , _data  :: SData
                      , _len   :: Integer
                      , _vlen  :: Float
                      , _tick  :: Integer
                      , _play  :: PlayMode
                      , _marks :: M.Map String Float
                      , _obufname :: String
                  }
                  deriving (Show,Eq)

instance NFData WSpace where
  rnf ws = rnf (_fname ws) `seq`
           rnf (_data ws) `seq`
           rnf (_len ws) `seq`
           rnf (_vlen ws) `seq`
           rnf (_tick ws) `seq`
           rnf (_play ws) `seq`
           rnf (_marks ws) `seq`
           rnf (_obufname ws) `seq`
           ()
