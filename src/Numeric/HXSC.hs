{-# LANGUAGE KindSignatures, DataKinds, FlexibleInstances #-}
module Numeric.HXSC (Round, Rounding) where

import Numeric.HXSC.Internal

data Rounding = Nearest
  | Up
  | Down
--  | Zero

newtype Round (rounding :: Rounding) x = Round x

instance Eq x => Eq (Round rounding x) where
  (Round x) == (Round y) = x == y

instance Ord x => Ord (Round rounding x) where
  compare (Round x) (Round y) = compare x y

minInt, maxInt :: Integer
minInt = fromIntegral (minBound :: Int)
maxInt = fromIntegral (maxBound :: Int)

minIntU, maxIntU :: Double
minIntU = fromIntU (minBound :: Int)
maxIntU = fromIntU (maxBound :: Int)

minIntD, maxIntD :: Double
minIntD = fromIntD (minBound :: Int)
maxIntD = fromIntD (maxBound :: Int)

-- TODO: This is probably rather sub-optimal in every way...
fromIntegerU :: Integer -> Double
fromIntegerU = go (fromIntU 0)
  where go acc n | minInt <= n && n <= maxInt = plusU acc (fromIntU (fromInteger n))
                 |   n > 0   = go (plusU acc maxIntU) (n - maxInt)
                 | otherwise = go (minusU acc minIntU) (n - minInt)

fromIntegerD :: Integer -> Double
fromIntegerD = go (fromIntD 0)
  where go acc n | minInt <= n && n <= maxInt = plusD acc (fromIntD (fromInteger n))
                 |   n > 0   = go (plusD acc maxIntD) (n - maxInt)
                 | otherwise = go (minusD acc minIntD) (n - minInt)

instance Num (Round 'Up Double) where
  (Round x) + (Round y) = Round $ plusU x y
  (Round x) - (Round y) = Round $ minusU x y
  (Round x) * (Round y) = Round $ multU x y
  fromInteger n = Round (fromIntegerU n)
  -- TODO: I believe no exact rounding is needed for these
  abs (Round x) = Round (abs x)
  signum (Round x) = Round (signum x)

instance Num (Round 'Down Double) where
  (Round x) + (Round y) = Round $ plusD x y
  (Round x) - (Round y) = Round $ minusD x y
  (Round x) * (Round y) = Round $ multD x y
  fromInteger n = Round (fromIntegerD n)
  -- TODO: I believe no exact rounding is needed for these
  abs (Round x) = Round (abs x)
  signum (Round x) = Round (signum x)

instance Num (Round 'Nearest Double) where
  (Round x) + (Round y) = Round $ x + y
  (Round x) - (Round y) = Round $ x - y
  (Round x) * (Round y) = Round $ x * y
  abs (Round x) = Round (abs x)
  signum (Round x) = Round (signum x)
  fromInteger n = Round (fromInteger n)
