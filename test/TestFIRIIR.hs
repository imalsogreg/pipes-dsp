module TestFIRIIR where

import Pipes.DSP

import Pipes
import qualified Data.Vector.Unboxed as U

sputteredTestData :: [U.Vector Double]
sputteredTestData = [ U.fromList [1,2]
                    , U.fromList [3,4,5,6,7,8]
                    , U.fromList [9]
                    , U.fromList [10, 11, 12]
                    ]

myIIR :: (Monad m) => Pipe Double Double m r
myIIR = iirP [(1, 0, 0, 1, 1, 1),(1, 0, 0, 1, 1, 1)]

myBiquad :: Monad m => Pipe Double Double m r
myBiquad = biquadDirect1P (1, 0, 0, 1, 1, 1)
