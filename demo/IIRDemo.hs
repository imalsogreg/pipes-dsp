module Main where

import Control.Proxy
import Control.Proxy.DSP
import Control.Proxy.Trans.Writer

import Control.Monad.Trans.Writer

import Graphics.HsCharts

import Data.Time


main :: IO ()
main = do
  t0 <- getCurrentTime
  myData <- xOut
  t1 <- getCurrentTime
  putStrLn $ "t1: " ++ show (diffUTCTime t0 t1)
  putStrLn $ show . snd $ myData
  t2 <- getCurrentTime
  putStrLn $ "t2: " ++ show (diffUTCTime t1 t2)
  myData2 <- xOut2
  t3 <- getCurrentTime
  putStrLn $ "t3: " ++ show (diffUTCTime t2 t3)
  putStrLn $ show . snd $ myData2
  t4 <- getCurrentTime
  putStrLn $ "t4: " ++ show (diffUTCTime t3 t4)

freq :: Double
freq = 10.0

tEnd :: Double
tEnd = 10.0

--ts :: Vector Double
--ts = let dt = 1/freq in generate (floor $ tEnd * freq) (\n -> dt * fromIntegral n)

ts :: [Double]
ts = [0,1/freq .. tEnd]

xIn :: (Proxy p, Monad m) => () -> Producer p Double m ()
xIn = (sawtoothWave 1 1 1) `sampledAt` freq


xOut :: Monad m => m ((),Sum Int)
xOut = runProxy $ runWriterK $ xIn >-> myIIR >-> takeB (32000*16) >-> lengthD

xOut2 :: Monad m => m ((),Sum Int)
xOut2 = runProxy $ runWriterK $ (simpleOscillator 5 1 0) `sampledAt` 1000 >-> myIIR' >-> takeB 32000 >-> lengthD

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