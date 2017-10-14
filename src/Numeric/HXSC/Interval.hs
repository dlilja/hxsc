{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Numeric.HXSC.Interval where

import Numeric.HXSC.Real
import Prelude hiding (elem)

data Interval a b = Interval { iInf :: {-# UNPACK #-} !a
                             , iSup :: {-# UNPACK #-} !b }
  deriving (Eq, Show)

(...) :: a -> a -> RoundedInterval a
(...) a b = Interval (Rounded a) (Rounded b)

type RoundedInterval a = Interval (Rounded 'Down a) (Rounded 'Up a)

data Position = Below
              | Intersects
              | Above

position :: Ord a => a -> RoundedInterval a -> Position
position pivot (Interval (Rounded inf) (Rounded sup)) | inf >= pivot = Above
                                                      | sup <= pivot = Below
                                                      | otherwise = Intersects

positionStrict :: Ord a => a -> RoundedInterval a -> Position
positionStrict pivot (Interval (Rounded inf) (Rounded sup)) | inf > pivot = Above
                                                            | sup < pivot = Below
                                                            | otherwise = Intersects

instance Num (RoundedInterval Double) where
  (Interval inf1 sup1) + (Interval inf2 sup2) =
    Interval (inf1 + inf2) (sup1 + sup2)

  (Interval inf1 sup1) - (Interval inf2 sup2) =
    Interval (inf1 + safeNegate sup2) (sup1 + safeNegate inf2)

  int1@(Interval inf1 sup1) * int2@(Interval inf2 sup2) =
    case position 0 int1 of
      Above ->
        case position 0 int2 of
          Above -> Interval (inf1 * inf2) (sup1 * sup2)
          Below -> Interval (unsafeRound sup1 * unsafeRound sup2) (unsafeRound inf1 * unsafeRound inf2)
          Intersects -> Interval (unsafeRound sup1 * inf2) (sup1 * sup2)
      Below ->
        case position 0 int2 of
          Below -> Interval (inf1 * inf2) (sup1 * sup2)
          Above -> Interval (inf1 * unsafeRound sup2) (sup1 * unsafeRound inf2)
          Intersects -> Interval (inf1 * unsafeRound sup2) (unsafeRound inf1 * unsafeRound inf2)
      Intersects ->
        case position 0 int2 of
          Above -> Interval (inf1 * unsafeRound sup2) (sup1 * sup2)
          Below -> Interval (unsafeRound sup1 * inf2) (unsafeRound inf1 * unsafeRound inf2)
          Intersects -> let lb = min (inf1 * unsafeRound sup2) (unsafeRound sup1 * inf2)
                            ub = max (unsafeRound inf1 * unsafeRound inf2) (sup1 * sup2)
                         in Interval lb ub

  abs int@(Interval inf sup) = case position 0 int of
    Above -> int
    Below -> Interval (unsafeRound (abs sup)) (unsafeRound (abs inf))
    Intersects -> Interval 0 (max (unsafeRound inf) sup)

  signum int = case position 0 int of
    Above -> -1
    Below ->  1
    Intersects -> Interval (-1) 1

  fromInteger n = Interval (fromInteger n) (fromInteger n)

instance Fractional (RoundedInterval Double) where
  fromRational x = Interval (fromRational x) (fromRational x)
  recip int@(Interval inf sup) =
    if not (elem int 0)
    then Interval (recip (unsafeRound sup)) (recip (unsafeRound inf))
    else case (inf == 0, sup == 0) of
      (True, True) -> error "NOOOOOOO"
      (True,   _ ) -> Interval (recip (unsafeRound sup)) (1 / 0)
      (  _ , True) -> Interval (- (1 / 0)) (recip (unsafeRound inf))
      _      -> Interval (- (1 / 0)) (1 / 0)

hull :: (Ord a, Ord b) => Interval a b -> Interval a b -> Interval a b
hull (Interval inf1 sup1) (Interval inf2 sup2) = Interval (min inf1 inf2) (max sup1 sup2)

cap :: Ord a => RoundedInterval a -> RoundedInterval a -> Maybe (RoundedInterval a)
cap (Interval (Rounded inf1) (Rounded sup1)) (Interval (Rounded inf2) (Rounded sup2))
  | sup1 < inf2 = Nothing
  | sup2 < inf1 = Nothing
  | otherwise = Just (Interval (Rounded (max inf1 inf2)) (Rounded (min sup1 sup2)))

subsetOf :: (Ord a, Ord b) => Interval a b -> Interval a b -> Bool
subsetOf (Interval inf1 sup1) (Interval inf2 sup2) = (inf1 <= inf2) && (sup2 <= sup1)

isSubset :: (Ord a, Ord b) => Interval a b -> Interval a b -> Bool
isSubset = flip subsetOf

singleton :: a -> RoundedInterval a
singleton a = Interval (Rounded a) (Rounded a)

elem :: Ord a => RoundedInterval a -> a -> Bool
elem int x = subsetOf int (singleton x)

midpoint :: RoundedInterval Double -> RoundedInterval Double
midpoint (Interval inf sup) = Interval ((inf + unsafeRound sup) / 2) ((unsafeRound inf + sup) / 2)

diameter :: RoundedInterval Double -> RoundedInterval Double
diameter (Interval inf sup) = Interval (unsafeRound sup - inf) (sup - unsafeRound inf)
