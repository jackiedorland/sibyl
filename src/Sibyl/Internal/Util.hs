{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Sibyl.Internal.Util where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B

type Text = String

strictlyIncreasing :: (U.Unbox a, Ord a) => U.Vector a -> Bool
strictlyIncreasing v
        | U.length v < 2 = True
        | otherwise      = U.and (U.zipWith (<) v (U.tail v))

bStrictlyIncreasing :: (Ord a) => B.Vector a -> Bool
bStrictlyIncreasing v
        | B.length v < 2 = True
        | otherwise      = B.and (B.zipWith (<) v (B.tail v))