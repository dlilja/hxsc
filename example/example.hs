module Main where

import Numeric.HXSC
import Numeric.HXSC.Internal

import Text.Printf
import qualified Data.Vector.Unboxed as V
import System.Random.MWC

import Control.DeepSeq (force)
import Control.Exception (evaluate)

import Criterion.Main

lconst :: a -> b -> b
lconst _ b = b

main :: IO ()
main = do xs  <- evaluate . force $ V.iterateN 1000000 (+0.1) 0
          let x = 2.0 :: Double
              y = 0.3 :: Double
              n = 9007199254740993 :: Int

          putStrLn ""
          printf "%d \\in (%.20f, %.20f)\n" n (fromIntD n) (fromIntU n)
          printf "sqrt(%f) \\in (%.20f, %.20f)\n" x (sqrtD x) (sqrtU x)
          printf "%f + %f \\in (%.20f, %.20f)\n" x y (plusD  x y) (plusU  x y)
          printf "%f + %f \\in (%.20f, %.20f)\n" x y (plusD' x y) (plusU' x y)

          defaultMain [
            -- bgroup "single" [ bench "nop"   $ nf (     id      ) (x, y)
            --                 , bench "plusD" $ nf (uncurry plusD) (x, y)
            --                 , bench "plusN" $ nf (uncurry plusN) (x, y)
            --                 , bench "plus." $ nf (uncurry  (+) ) (x, y)
            --                 , bench "plusU" $ nf (uncurry plusU) (x, y)
            --                 ]
            -- ,
            bgroup "multiple" [ bench "nop"   $ nf (V.foldl' const (0 :: Double)) xs
                              , bench "plus." $ nf (V.foldl'  (+)  0) xs
                              , bench "plusN" $ nf (V.foldl' plusN 0) xs
                              , bench "plusU" $ nf (V.foldl' plusU 0) xs
                              , bench "plusU'" $ nf (V.foldl' plusU' 0) xs
                              -- , bench "plusUV" $ nf vsumU xs
                              , bench "plusD" $ nf (V.foldl' plusD 0) xs
                              -- , bench "plusDV" $ nf vsumD xs
                              , bench "plusD'" $ nf (V.foldl' plusD' 0) xs
                              ]]
