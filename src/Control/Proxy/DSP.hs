module Control.Proxy.DSP where

import Control.Proxy
import qualified Data.Vector.Unboxed as V

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
firstOrderP b0 b1 a1 g () = runIdentityP $ loop 0 0
  where
    gInv = 1 / g
    loop x1 y1 = do
      x0 <- request ()
      let y0 = (b0*x0 + b1*x1 - a1*y1) * gInv
      respond y0
      loop x0 y0

-- |All-pass filter pipe implemented as special case
-- of the biquad filter filter
allPassSectionP :: (Proxy p, Floating x, Monad m) => () -> Pipe p x x m r
allPassSectionP = biquadDirect1P 1 0 0 0 0 1

-- |Construct an iir filter from a series of
-- second-order sections
iirP :: (Proxy p, Floating x, Monad m)
        => [(x, x, x, x, x, x)] -- ^ list of biquad params (b0,b1,b2,a1,a2,g)
        -> () -> Pipe p x x m r
iirP biquadParams = foldr (\a b -> (a >-> b)) pull biquads
  where
    biquads = map
              (\(b0, b1, b2, a1, a2, g) -> biquadDirect1P b0 b1 b2 a1 a2 g)
              biquadParams

myBiquad :: (Proxy p, Monad m) => () -> Pipe p Double Double m r
myBiquad = biquadDirect1P 1 0 0 1 1 1

myIIR :: (Proxy p, Monad m) => () -> Pipe p Double Double m r
myIIR = iirP [(1, 0, 0, 1, 1, 1),(1, 0, 0, 1, 1, 1)]
--firP :: (Proxy p)

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
        respond toSend
        loop (theRest, bufferLen - chunkSize)
      | otherwise = do
        newBuffer <- request ()
        loop (buffer V.++ newBuffer, bufferLen + V.length newBuffer)


exampleUse = runProxy $ fromListS sputterTestData >-> unSputter 5 >-> printD