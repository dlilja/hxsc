{-# LANGUAGE RecordWildCards, BangPatterns #-}
{-# LANGUAGE GHCForeignImportPrim, ForeignFunctionInterface, MagicHash, UnboxedTuples, UnliftedFFITypes #-}
module Numeric.HXSC.Internal where

import GHC.Prim
import GHC.Exts
import GHC.Word

import Control.Exception (evaluate)

-- import Data.Vector.Unboxed as V

-------------------------------------
-- WARNING WARNING WARNING WARNING --
-------------------------------------
-- ALL THIS CODE ASSUMES x64!!!!!! --
-------------------------------------

foreign import prim "plusu64U"
  plusu64U# :: Double# -> Double# -> Double#

foreign import prim "plusd64U"
  plusd64U# :: Double# -> Double# -> Double#

foreign import prim "minusu64U"
  minusu64U# :: Double# -> Double# -> Double#

foreign import prim "minusd64U"
  minusd64U# :: Double# -> Double# -> Double#

foreign import prim "multu64U"
  multu64U# :: Double# -> Double# -> Double#

foreign import prim "multd64U"
  multd64U# :: Double# -> Double# -> Double#

foreign import prim "divu64U"
  divu64U# :: Double# -> Double# -> Double#

foreign import prim "divd64U"
  divd64U# :: Double# -> Double# -> Double#

foreign import prim "fromintu64U"
  fromintu64U# :: Int# -> Double#

foreign import prim "fromintd64U"
  fromintd64U# :: Int# -> Double#

foreign import prim "sqrtu64S"
  sqrtu64S# :: Double# -> Double#

foreign import prim "sqrtd64S"
  sqrtd64S# :: Double# -> Double#

foreign import prim "sqrtu64U"
  sqrtu64U# :: Double# -> Double#

foreign import prim "sqrtd64U"
  sqrtd64U# :: Double# -> Double#

---------------------------------------

{-# INLINE plusU #-}
plusU :: Double -> Double -> Double
plusU (D# x) (D# y) = D# (plusu64U# x y)

{-# INLINE plusD #-}
plusD :: Double -> Double -> Double
plusD (D# x) (D# y) = D# (plusd64U# x y)

-- vsumU :: V.Vector Double -> Double
-- vsumU = V.foldl' plusU 0

-- vsumD :: V.Vector Double -> Double
-- vsumD = V.foldl' plusD 0

{-# INLINE minusU #-}
minusU :: Double -> Double -> Double
minusU (D# x) (D# y) = D# (minusu64U# x y)

{-# INLINE minusD #-}
minusD :: Double -> Double -> Double
minusD (D# x) (D# y) = D# (minusd64U# x y)

{-# INLINE multU #-}
multU :: Double -> Double -> Double
multU (D# x) (D# y) = D# (multu64U# x y)

{-# INLINE multD #-}
multD :: Double -> Double -> Double
multD (D# x) (D# y) = D# (multd64U# x y)

{-# INLINE divU #-}
divU :: Double -> Double -> Double
divU (D# x) (D# y) = D# (divu64U# x y)

{-# INLINE divD #-}
divD :: Double -> Double -> Double
divD (D# x) (D# y) = D# (divd64U# x y)

{-# INLINE fromIntU #-}
fromIntU :: Int -> Double
fromIntU (I# n) = D# (fromintu64U# n)

{-# INLINE fromIntD #-}
fromIntD :: Int -> Double
fromIntD (I# n) = D# (fromintd64U# n)

{-# INLINE sqrtU #-}
sqrtU :: Double -> Double
sqrtU (D# x) = D# (sqrtu64U# x)

{-# INLINE sqrtD #-}
sqrtD :: Double -> Double
sqrtD (D# x) = D# (sqrtd64U# x)

--------- Aux stuff -------------------

foreign import prim "getmxcsr"
  -- getmxcsr# :: Word64# -> Word64#
  getmxcsr# :: Word# -> Word#

foreign import prim "plusn64U"
  plusn64U# :: Double# -> Double# -> Double#

foreign import prim "plusu64S"
  plusu64S# :: Double# -> Double# -> Double#

foreign import prim "plusd64S"
  plusd64S# :: Double# -> Double# -> Double#

{-# INLINE plusU' #-}
plusU' :: Double -> Double -> Double
plusU' (D# x) (D# y) = D# (plusu64S# x y)

{-# INLINE plusD' #-}
plusD' :: Double -> Double -> Double
plusD' (D# x) (D# y) = D# (plusd64S# x y)

{-# INLINE plusN #-}
plusN :: Double -> Double -> Double
plusN (D# x) (D# y) = D# (plusn64U# x y)

getmxcsr :: IO Word64
getmxcsr = case 0 of (W64# zero) -> evaluate (W64# (getmxcsr# zero))
