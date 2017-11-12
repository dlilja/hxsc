module Main where

import Numeric.HXSC
import Numeric.HXSC.Real
import Numeric.HXSC.Interval
import Prelude hiding ((**))

main :: IO ()
main = do
  let x = (-2 :: Double) ... (2 :: Double)
  print x
  print (testFunc x)
