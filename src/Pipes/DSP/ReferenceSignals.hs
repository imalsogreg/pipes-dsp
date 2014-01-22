module Pipes.DSP.ReferenceSignals where

import Pipes
import qualified Pipes.Prelude as P

type Time = Double
type SignalValue = Double
type FreqHz = Double
type Coef = Double
type Phase = Double

-- |Signal producer
signalP :: Monad m
           => (Time -> SignalValue) -- ^ x(t)
           -> FreqHz -- ^ Sampling Frequency (Hz)
           -> Producer SignalValue m ()
signalP fn freq = each [0..] >-> P.map (fn . (* dt))
  where dt = 1 / freq

-- |Signal producer renamed for infix use
-- (ie (simpleOscillator 100 pi) `sampledAt` 100)
sampledAt :: Monad m
             => (Time -> SignalValue) -- ^ x(t)
             -> FreqHz -- ^ Sampling Frequency (Hz)
             -> Producer SignalValue m ()
sampledAt = signalP

-- |Produce values sampled from the function along with the sampling
-- times (useful for plotting)
signalWithTime :: Monad m
                  => (Time -> SignalValue)
                  -> FreqHz
                  -> Producer (Time,SignalValue) m ()
signalWithTime fn freq = (each [0..]) >-> P.map (\n -> (n*dt, fn(n*dt)))
  where dt = 1 / freq

simpleOscillator :: Coef -- ^ amplitude
                 -> FreqHz -- ^ frequency
                 -> Phase -- ^ phase offset
                 -> (Time -> SignalValue) -- ^ x(t)
simpleOscillator a freq p = \t -> a * cos(2*pi*freq*t - p)

sawtoothWave :: Coef -- ^ amplitude
             -> FreqHz -- ^ frequency
             -> Time -- ^ time offset
             -> (Time -> SignalValue) -- ^ x(t)
sawtoothWave a f t0 =
  \t -> a * 2 * ((t-t0)*f - (fromIntegral . (floor :: Double -> Int) )
                 (0.5 + (t-t0)*f))
