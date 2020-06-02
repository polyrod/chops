{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL as SDL
import SDL.Cairo
import Linear.V2 (V2(..))
import Graphics.Rendering.Cairo.Canvas
import qualified Sound.File.Sndfile               as SF
import qualified Sound.File.Sndfile.Buffer.Vector as BV
import qualified Data.Vector.Storable             as SV

import System.IO
import System.Environment
import Control.Monad

import Chops.Cpu
import Chops.Audio


drawSample (D a y w h) s = do
  let ps =  map calcP [0..(floor w)]
  mapM_ point ps
  zipWithM line ps (tail ps)

  where 
    t x = fromIntegral x / w
    idx x = floor $ t x * fromIntegral (SV.length s - 1)
    val x = 4 * (s SV.! idx x)
    calcP x = V2 (fromIntegral x) ((h / 2) + ((h / 2) * val x)) 
  

main :: IO ()
main = do
  args <- getArgs
  loadNRun $ args !! 0
--  go 100


 

go :: Double -> IO ()            --
go p = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  smp <- loadSample "audio.wav"
  let winConfig = SDL.defaultWindow { SDL.windowPosition = SDL.Absolute (SDL.P (V2 0 15))
                                    , SDL.windowInitialSize = V2 1366 200 }

      rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True }
  SDL.initializeAll
  window <- SDL.createWindow "cairo-canvas using SDL2" winConfig 
  renderer <- SDL.createRenderer window (-1) rdrConfig
  texture <- createCairoTexture' renderer window
  let go' a = do 
                withCairoTexture' texture $ runCanvas $ disp smp a
                SDL.copy renderer texture Nothing Nothing
                SDL.present renderer
                putStr $ show a ++ "> "
                c <- getChar
                putStr "\n"
                case c of
                 'm' -> go' $ inRange 0 1366 a (+10)
                 'n' -> go' $ inRange 0 1366 a (flip (-) 10)
                 'q' -> pure ()
                 _ ->  go' a
  go' p          

inRange l h a f = let fa = f a in if l <= fa && fa <= h then fa else a
  
disp smp p  = do
              let font = Font "Helvetica" 8 False False
              background $ gray 102
              fill $ red 255 !@ 128
              noStroke
              rect $ D 0 75 10 50
              stroke $ red 255 !@ 128
              line (V2 0 100) (V2 1366 100)
              textFont font
              text "Hello World" (V2 100 92)
              stroke $ blue 255 !@ 128
              drawSample (D 0 200 1366 200) smp
              line (V2 p 0) (V2 p 200)


