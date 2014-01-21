{-# LANGUAGE BangPatterns #-}

module Pipes.DSP.FIRIIR where

import Pipes

-- |BiquadCoeffs (b0,b1,b2,a0,a1,a2)
-- The transfer function is defined as
-- H(z) = (b0 + b1*z^-1 + b2*z-2) / (a0 + a1*z^-1 + a2*z^-2)
type BiquadCoeffs = (Double, Double, Double, Double, Double, Double)

-- |First order direct form coefficients (b0, b1, a0, a1)
-- Transfer function: H(z) = (b0 + b1*z-1) / (a0 + a1*z^-1)
type FirstOrderCoeffs = (Double, Double, Double, Double)

-- |Pass a stream of Doubles through a first-order filter
-- (b0, b1, a1, filterGain)
firstOrderP :: Monad m
               => FirstOrderCoeffs -> Pipe Double Double m r
firstOrderP (b0, b1, a1, g) = loop 0 0
  where
    gInv = 1 / g
    loop x1 y1 = do
      x0 <- await
      let y0 = (b0*x0 + b1*x1 - a1*y1) * gInv
      _ <- yield y0
      loop x0 y0

-- |Biquad filter pipe
-- y0 = (b0 x0 + b1 x1 + b2 x2 - a1 y1 - a2 y2) / g
{-# INLINE biquadDirect1P #-}
biquadDirect1P :: (Monad m) => BiquadCoeffs -> Pipe Double Double m r
biquadDirect1P (b0, b1, b2, a0, a1, a2) = loop 0 0 0 0
  where
    (b0',b1',b2',a1',a2') = (b0/a0, b1/a0, b2/a0, a1/a0, a2/a0)
    {-# INLINE loop #-}
    loop !x1 !x2 !y1 !y2 = do
      x0 <- await
      let y0 = (b0'*x0 + b1'*x1 + b2'*x2 - a1'*y1 - a2'*y2)
      _ <- yield y0
      loop x0 x1 y0 y1

-- |Construct a higher-order fir/iir filter from
-- a series of second-order sections
iirP :: Monad m => 
        [BiquadCoeffs] -- ^ list of biquad params [(b0,b1,b2,a0,a1,a2)]
        -> Pipe Double Double m r
iirP biquadParams = foldr (>->) cat (map biquadDirect1P biquadParams)

{- What is this for?
iirP' :: Monad m => [BiquadCoeffs]
         -> Pipe p Double Double m r
iirP' biquadParams = pull
-}

-- |All-pass filter pipe implemented as special case
-- of the biquad filter filter
allPassSectionP :: Monad m => Pipe Double Double m r
allPassSectionP = biquadDirect1P (1, 0, 0, 0, 0, 1)
