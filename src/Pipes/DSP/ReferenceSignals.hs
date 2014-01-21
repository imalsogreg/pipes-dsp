module Pipes.DSP.ReferenceSignals where

import Pipes
import qualified Pipes.Prelude as P

-- |Signal producer
signalP :: Monad m
           => (Double -> Double) -- ^ x(t)
           -> Double -- ^ Sampling Frequency (Hz)
           -> Producer Double m ()
signalP fn freq = each [0..] >-> P.map (fn . (* dt))
  where dt = 1 / freq

-- |Signal producer renamed for infix use
-- (ie (simpleOscillator 100 pi) `sampledAt` 100)
sampledAt :: Monad m
             => (Double -> Double) -- ^ x(t)
             -> Double -- ^ Sampling Frequency (Hz)
             -> Producer Double m ()
sampledAt = signalP

-- |Produce values sampled from the function along with the sampling
-- times (useful for plotting)
signalWithTime :: Monad m
                  => (Double -> Double)
                  -> Double
                  -> Producer (Double,Double) m ()
signalWithTime fn freq = (each [0..]) >-> P.map (\n -> (n*dt, fn(n*dt)))
  where dt = 1 / freq


simpleOscillator :: Double -- ^ amplitude
                 -> Double -- ^ frequency
                 -> Double -- ^ phase offset
                 -> (Double -> Double) -- ^ x(t)
simpleOscillator a freq p = \t -> a * cos(2*pi*freq*t - p)

sawtoothWave :: Double -- ^ amplitude
             -> Double -- ^ frequency
             -> Double -- ^ time offset
             -> (Double -> Double) -- ^ x(t)
sawtoothWave a f t0 =
  \t -> a * 2 * ((t-t0)*f - (fromIntegral . (floor :: Double -> Int) )
                 (0.5 + (t-t0)*f))
