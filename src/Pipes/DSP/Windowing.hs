module Pipes.DSP.Windowing where

import Prelude hiding ((++))
import Pipes
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((++))
import Control.Monad

toWindows :: (Monad m) => Int -> Pipe Double (U.Vector Double) m r
toWindows nSamps = forever $ do
  xs <- replicateM nSamps await
  yield $ U.fromList xs

toSlidingWindow :: Monad m => Int
                 -> Int
                 -> Pipe Double (U.Vector Double) m ()
toSlidingWindow winLength slideLength = go xs0
  where
    go xs = do
      newXs <- replicateM slideLength await
      let xs' = U.drop slideLength $
                xs ++ U.fromList newXs
      yield xs'
      go xs'
    xs0 = U.fromList $ map fromIntegral [0..winLength-1]
