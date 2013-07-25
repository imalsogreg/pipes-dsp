module Control.Proxy.DSP where

import Control.Proxy

-- |Biquad filter pipe
-- y0 = (b0 x0 + b1 x1 + b2 x2 - a1 y1 - a2 y2) / g
biquadDirect1P :: (Proxy p, Floating x, Monad m)
        => x -- ^ b0 (most recent input's coefficient
        -> x -- ^ b1
        -> x -- ^ b2
        -> x -- ^ a1
        -> x -- ^ a2
        -> x -- ^ filter gain
        -> () -> Pipe p x x m r
biquadDirect1P b0 b1 b2 a1 a2 g () = runIdentityP $ loop 0 0 0 0
  where
    gInv = 1 / g
    loop x1 x2 y1 y2 = do
      x0 <- request ()
      let y0 = (b0*x0 + b1*x1 + b2*x2 - a1*y1 - a2*y2) * gInv
      respond y0
      loop x0 x1 y0 y1

firstOrderP :: (Proxy p, Floating x, Monad m)
               => x -- ^ b0 (most recent input's coefficient
               -> x -- ^ b1
               -> x -- ^ a1 (n-1  output's coefficient)
               -> x -- ^ filter gain
               -> () -> Pipe p x x m r
firstOrderP b0 b1 a1 g () = runIdentitpP $ loop 0 0
  where
    gInv = 1 / g
    loop x1 y1 = do
      x0 <- request ()
      let y0 = (b0*x0 + b1*x1 - a1*y1) * gInf
      respond y0
      loop x0 y0

-- |All-pass filter pipe implemented as special case
-- of the biquad filter filter
allPassSectionP :: (Proxy p, Floating x, Monad m) => () -> Pipe p x x m r
allPassSectionP = biquadDirect1P 1 0 0 0 0 1

iirP :: 

--firP :: (Proxy p) 