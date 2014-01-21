module Main where

import Pipes.DSP
import TestFIRIIR

import Pipes
import qualified Pipes.Prelude as P
import Graphics.Rendering.Chart
import Data.Time

main :: IO ()
main = do
  t0 <- getCurrentTime
  myData <- runEffect $ P.length (xOut >-> P.take 1000)
  t1 <- getCurrentTime
  putStrLn . show $ diffUTCTime t1 t0

freq :: Double
freq = 10.0

tEnd :: Double
tEnd = 10.0

--ts :: Vector Double
--ts = let dt = 1/freq in generate (floor $ tEnd * freq) (\n -> dt * fromIntegral n)

ts :: [Double]
ts = [0,1/freq .. tEnd]

xIn :: Monad m => Producer Double m ()
xIn = (sawtoothWave 1 1 1) `sampledAt` freq

xOut :: Monad m => Producer Double m ()
xOut = xIn >-> myIIR

windowW :: Float
windowW = 500

windowH :: Float
windowH = 200

chartBuffer :: Float
chartBuffer = 10

chartW :: Float
chartW = windowW - 2*chartBuffer

chartH :: Float
chartH = windowH - 2*chartBuffer
