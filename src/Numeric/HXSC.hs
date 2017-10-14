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
