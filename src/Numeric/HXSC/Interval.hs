{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Numeric.HXSC.Interval where

import Numeric.HXSC.Real

data Interval a b = Interval { iInf :: {-# UNPACK #-} !a
                             , iSup :: {-# UNPACK #-} !b }
  deriving (Eq)

(...) :: a -> b -> Interval a b
(...) a b = Interval a b

data IntervalType = NonPositive
                  | Intersects0
                  | NonNegative

intervalType :: (Num a, Num b, Ord a, Ord b) => Interval a b -> IntervalType
intervalType (Interval inf sup) | inf >= 0  = NonNegative
                                | sup <= 0  = NonPositive
                                | otherwise = Intersects0

instance Num (Interval (Rounded 'Down Double) (Rounded 'Up Double)) where
  (Interval inf1 sup1) + (Interval inf2 sup2) =
    Interval (inf1 + inf2) (sup1 + sup2)

  (Interval inf1 sup1) - (Interval inf2 sup2) =
    Interval (inf1 + safeNegate sup2) (sup1 + safeNegate inf2)

  int1@(Interval inf1 sup1) * int2@(Interval inf2 sup2) =
    case intervalType int1 of
      NonNegative ->
        case intervalType int2 of
          NonNegative -> Interval (inf1 * inf2) (sup1 * sup2)
          NonPositive -> Interval (unsafeRound sup1 * unsafeRound sup2) (unsafeRound inf1 * unsafeRound inf2)
          Intersects0 -> Interval (unsafeRound sup1 * inf2) (sup1 * sup2)
      NonPositive ->
        case intervalType int2 of
          NonPositive -> Interval (inf1 * inf2) (sup1 * sup2)
          NonNegative -> Interval (inf1 * unsafeRound sup2) (sup1 * unsafeRound inf2)
          Intersects0 -> Interval (inf1 * unsafeRound sup2) (unsafeRound inf1 * unsafeRound inf2)
      Intersects0 ->
        case intervalType int2 of
          NonNegative -> Interval (inf1 * unsafeRound sup2) (sup1 * sup2)
          NonPositive -> Interval (unsafeRound sup1 * inf2) (unsafeRound inf1 * unsafeRound inf2)
          Intersects0 -> let lb = min (inf1 * unsafeRound sup2) (unsafeRound sup1 * inf2)
                             ub = max (unsafeRound inf1 * unsafeRound inf2) (sup1 * sup2)
                         in Interval lb ub

  abs int@(Interval inf sup) = case intervalType int of
    NonNegative -> int
    NonPositive -> Interval (unsafeRound (abs sup)) (unsafeRound (abs inf))
    Intersects0 -> Interval 0 (max (unsafeRound inf) sup)

  signum int = case intervalType int of
    NonNegative -> -1
    NonPositive ->  1
    Intersects0 -> Interval (-1) 1

  fromInteger n = Interval (fromInteger n) (fromInteger n)
