{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Numeric.HXSC.Real where

import Numeric.HXSC.Internal

data Rounding = Up | Down

newtype Rounded (direction :: Rounding) a = Rounded a

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

swapRounding :: Rounding -> Rounding
swapRounding Up = Down
swapRounding Down = Up

-- safeNegate :: Rounded d Double -> Rounded (swapRounding d) Double
-- safeNegate (Rounded x) = Rounded (0-x)

unsafeRound :: Rounded d a -> Rounded d' a
unsafeRound (Rounded x) = Rounded x

class SafeNegate a where
  type Negated a
  safeNegate :: a -> Negated a

instance SafeNegate (Rounded 'Up Double) where
  type Negated (Rounded 'Up Double) = Rounded 'Down Double
  safeNegate (Rounded x) = Rounded (0-x)

instance SafeNegate (Rounded 'Down Double) where
  type Negated (Rounded 'Down Double) = Rounded 'Up Double
  safeNegate (Rounded x) = Rounded (0-x)

instance Eq x => Eq (Rounded rounding x) where
  (Rounded x) == (Rounded y) = x == y

instance Ord x => Ord (Rounded rounding x) where
  compare (Rounded x) (Rounded y) = compare x y

instance Num (Rounded 'Up Double) where
  (Rounded x) + (Rounded y) = Rounded $ plusU x y
  (Rounded x) - (Rounded y) = Rounded $ minusU x y
  (Rounded x) * (Rounded y) = Rounded $ multU x y
  fromInteger n = Rounded (fromIntegerU n)
  -- TODO: I believe no exact rounding is needed for these
  abs (Rounded x) = Rounded (abs x)
  signum (Rounded x) = Rounded (signum x)
  negate = error "Negate is not well defined for rounded numbers"

instance Num (Rounded 'Down Double) where
  (Rounded x) + (Rounded y) = Rounded $ plusD x y
  (Rounded x) - (Rounded y) = Rounded $ minusD x y
  (Rounded x) * (Rounded y) = Rounded $ multD x y
  fromInteger n = Rounded (fromIntegerD n)
  -- TODO: I believe no exact rounding is needed for these
  abs (Rounded x) = Rounded (abs x)
  signum (Rounded x) = Rounded (signum x)
  negate = error "Negate is not well defined for rounded numbers"

-- instance Num (Rounded 'Near Double) where
--   (Rounded x) + (Rounded y) = Rounded $ x + y
--   (Rounded x) - (Rounded y) = Rounded $ x - y
--   (Rounded x) * (Rounded y) = Rounded $ x * y
--   abs (Rounded x) = Rounded (abs x)
--   signum (Rounded x) = Rounded (signum x)
--   fromInteger n = Rounded (fromInteger n)
