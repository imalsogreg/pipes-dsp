{-# LANGUAGE BangPatterns #-}

module Control.Proxy.DSP where

import Control.Proxy
import qualified Data.Vector.Unboxed as V
import Data.Fixed (mod')

-- |BiquadCoeffs (b0,b1,b2,a0,a1,a2)
-- The transfer function is defined as
-- H(z) = (b0 + b1*z^-1 + b2*z-2) / (a0 + a1*z^-1 + a2*z^-2)
type BiquadCoeffs = (Double, Double, Double, Double, Double, Double)

-- |First order direct form coefficients (b0, b1, a0, a1)
-- Transfer function: H(z) = (b0 + b1*z-1) / (a0 + a1*z^-1)
type FirstOrderCoeffs = (Double, Double, Double, Double)

-- |Biquad filter pipe
-- y0 = (b0 x0 + b1 x1 + b2 x2 - a1 y1 - a2 y2) / g
{-# INLINE biquadDirect1P #-}
biquadDirect1P :: (Proxy p,  Monad m)
        => BiquadCoeffs
        -> () -> Pipe p Double Double m r
biquadDirect1P (b0, b1, b2, a0, a1, a2) () = runIdentityP $ loop 0 0 0 0
  where
    !(!b0',!b1',!b2',!a1',!a2') = (b0/a0, b1/a0, b2/a0, a1/a0, a2/a0)
    {-# INLINE loop #-}
    loop !x1 !x2 !y1 !y2 = do
      x0 <- request ()
      let y0 = (b0'*x0 + b1'*x1 + b2'*x2 - a1'*y1 - a2'*y2)
      _ <- respond y0
      loop x0 x1 y0 y1

firstOrderP :: (Proxy p,  Monad m)
               => FirstOrderCoeffs -> () -> Pipe p Double Double m r
firstOrderP (b0, b1, a1, g) () = runIdentityP $ loop 0 0
  where
    gInv = 1 / g
    loop x1 y1 = do
      x0 <- request ()
      let y0 = (b0*x0 + b1*x1 - a1*y1) * gInv
      _ <- respond y0
      loop x0 y0

-- |All-pass filter pipe implemented as special case
-- of the biquad filter filter
allPassSectionP :: (Proxy p, Monad m) => () -> Pipe p Double Double m r
allPassSectionP = biquadDirect1P (1, 0, 0, 0, 0, 1)

-- |Construct an iir filter from a series of
-- second-order sections
iirP :: (Proxy p, Monad m)
        => [BiquadCoeffs] -- ^ list of biquad params (b0,b1,b2,a1,a2,g)
        -> () -> Pipe p Double Double m r
iirP biquadParams = foldr (>->) pull (map biquadDirect1P biquadParams)

iirP' :: (Proxy p, Monad m)
         => [BiquadCoeffs]
         -> () -> Pipe p Double Double m r
iirP' biquadParams = pull

sputterTestData :: [V.Vector Double]
sputterTestData = [ V.fromList [1,2]
                  , V.fromList [3,4,5,6,7,8]
                  , V.fromList [9]
                  , V.fromList [10, 11, 12]
                  ]

unSputter :: (Proxy p, Floating x, V.Unbox x, Monad m)
             => Int -> () -> Pipe p (V.Vector x) (V.Vector x) m r
unSputter chunkSize () = runIdentityP $ loop (V.empty, 0)
  where
    loop (buffer, bufferLen)
      | bufferLen >= chunkSize = do
        let toSend  = V.take chunkSize buffer
            theRest = V.drop chunkSize buffer
        _ <- respond toSend
        loop (theRest, bufferLen - chunkSize)
      | otherwise = do
        newBuffer <- request ()
        loop (buffer V.++ newBuffer, bufferLen + V.length newBuffer)

-- |Signal producer
signalS :: (Proxy p, Monad m)
           => (Double -> Double) -- ^ x(t)
           -> Double -- ^ Sampling Frequency (Hz)
           -> () -> Producer p Double m ()
signalS fn freq = fromListS [0..] >-> mapD (\n -> fn (n* dt))
  where dt = 1 / freq

-- |Signal producer renamed for infix use
-- (ie (simpleOscillator 100 pi) `sampledAt` 100)
sampledAt :: (Proxy p, Monad m)
             => (Double -> Double) -- ^ x(t)
             -> Double -- ^ Sampling Frequency (Hz)
             -> () -> Producer p Double m ()
sampledAt = signalS

-- |Produce values sampled from the function along with the sampling
-- times (useful for plotting)
signalWithTime :: (Proxy p, Monad m)
                  => (Double -> Double)
                  -> Double
                  -> () -> Producer p (Double,Double) m ()
signalWithTime fn freq = fromListS [0..] >-> mapD (\n -> (n*dt, fn(n*dt)))
  where dt = 1 / freq


   

simpleOscillator :: Double -- ^ amplitude
                 -> Double -- ^ frequency
                 -> Double -- ^ phase offset
                 -> (Double -> Double) -- ^ x(t)
simpleOscillator a w p = \t -> a * cos(2*pi*w*t - p)

sawtoothWave :: Double -- ^ amplitude
             -> Double -- ^ frequency
             -> Double -- ^ time offset
             -> (Double -> Double) -- ^ x(t)
sawtoothWave a f t0 = \t -> a * 2 * ((t-t0)*f - (fromIntegral . floor) (0.5 + (t-t0)*f))

exampleUse :: IO ()
exampleUse = runProxy $ fromListS sputterTestData >-> unSputter 5 >-> printD


myIIR :: (Proxy p, Monad m) => () -> Pipe p Double Double m r
myIIR = iirP [(1, 0, 0, 1, 1, 1),(1, 0, 0, 1, 1, 1)]
--firP :: (Proxy p)

myIIR' :: (Proxy p, Monad m) => () -> Pipe p Double Double m r
myIIR' = iirP' []

myBiquad :: (Proxy p, Monad m) => () -> Pipe p Double Double m r
myBiquad = biquadDirect1P (1, 0, 0, 1, 1, 1)
