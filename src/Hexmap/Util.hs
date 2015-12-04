module Hexmap.Util where

import           Linear

type Radians = Double

m22 :: a -> a -> a -> a -> M22 a
m22 a b c d = V2 (V2 a b) (V2 c d)

sqrt_3, sqrt_3_2, sqrt_3_3 :: Double
sqrt_3 = sqrt 3
sqrt_3_2 = sqrt_3 / 2
sqrt_3_3 = sqrt_3 / 3
