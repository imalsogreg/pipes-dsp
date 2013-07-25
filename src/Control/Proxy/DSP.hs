module Control.Proxy.DSP where

import Control.Proxy

-- |Biquad filter pipe
-- y0 = (a0 x0 + a1 x1 + a2 x2 - b1 y1 - b2 y2) / g
sosP :: (Proxy p, Floating x, Monad m)
        => x -- ^ a0 (most recent input's coefficient
        -> x -- ^ a1
        -> x -- ^ a2
        -> x -- ^ y1
        -> x -- ^ y2
        -> x -- ^ filter gain
        -> () -> Pipe p x x m r
sosP a0 a1 a2 b1 b2 g () = runIdentityP $ loop 0 0 0 0
  where
    gInv = 1 / g
    loop x1 x2 y1 y2 = do
      x0 <- request ()
      let y0 = (a0*x0 + a1*x1 + a2*x2 - b1*y1 - b2*y2) * gInv
      respond y0
      loop x0 x1 y0 y1

allPassP :: (Proxy p, Floating x, Monad m) => () -> Pipe p x x m r
allPassP = sosP 1 0 0 0 0 1

--firP :: (Proxy p) 