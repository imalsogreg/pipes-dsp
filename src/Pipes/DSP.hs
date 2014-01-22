{-# LANGUAGE BangPatterns #-}

module Pipes.DSP (
    module Pipes.DSP.ReferenceSignals
  , module Pipes.DSP.FIRIIR
  , module Pipes.DSP.Windowing
  , unSputter
)
         where

import Pipes.DSP.ReferenceSignals
import Pipes.DSP.FIRIIR
import Pipes.DSP.Windowing

import Pipes
import qualified Data.Vector.Unboxed as V


unSputter :: (Floating x, V.Unbox x, Monad m)
             => Int -> Pipe (V.Vector x) (V.Vector x) m r
unSputter chunkSize = loop (V.empty, 0)
  where
    loop (buffer, bufferLen)
      | bufferLen >= chunkSize = do
        let toSend  = V.take chunkSize buffer
            theRest = V.drop chunkSize buffer
        _ <- yield toSend
        loop (theRest, bufferLen - chunkSize)
      | otherwise = do
        newBuffer <- await
        loop (buffer V.++ newBuffer, bufferLen + V.length newBuffer)




