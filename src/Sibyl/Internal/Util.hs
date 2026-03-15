{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Sibyl.Internal.Util where

import qualified Data.Vector.Unboxed as U

type Text = String

strictlyIncreasing :: (U.Unbox a, Ord a) => U.Vector a -> Bool
strictlyIncreasing v
        | U.length v < 2 = True
        | otherwise      = U.and (U.zipWith (<) v (U.tail v))

unsafeFromEither :: Show e => String -> Either e a -> a
unsafeFromEither context result =
  case result of
    Left err -> error (context ++ ": " ++ show err)
    Right value -> value