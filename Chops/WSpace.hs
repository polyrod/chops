module Chops.WSpace where

import qualified Data.Vector.Storable             as SV
import qualified Data.Map                         as M
import Data.IORef

data PlayMode = Stop | Fwd | Bwd
                  deriving (Show,Eq)

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


