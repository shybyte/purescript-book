module Solution where

import Prelude
import Data.Picture
import Data.Maybe
import Math


factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)


binomialCoefficients :: Int -> Int -> Int
binomialCoefficients n 0 = 1
binomialCoefficients n k
  | n==k = 1
  | otherwise = (binomialCoefficients (n-1) (k-1)) + (binomialCoefficients (n-1) k)



type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }


sameCity :: Person -> Person -> Boolean
sameCity {address: {city: city1}} {address: {city: city2}} =
  city1 == city2


fromSingleton ::forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton defaultValue _ = defaultValue


import Data.Array.Unsafe (tail)


isTwin :: Array Int -> Boolean
isTwin xs =
  case xs of
    [a,b] | a == b -> true
          | a /= b -> false
    _              -> false



centeredCircle = Circle (Point {x: 0.0, y:0.0}) 10.0


scale2 :: Shape -> Shape
scale2 (Circle c r) = Circle c (r/2.0)
scale2 (Rectangle c w h) = Rectangle c (w/2.0) (h/2.0)
scale2 (Line (Point{x: x1, y: y1}) (Point{x: x2, y: y2})) =
  Line ( Point {x: x1', y:y1'}) ( Point {x: x2', y: y2'})
  where
    centerX = (x1+x2)/2.0
    centerY = (y1+y2)/2.0
    scale2ToCenter z center = (z-center)/2.0 + center
    x1' = scale2ToCenter x1 centerX
    x2' = scale2ToCenter x2 centerX
    y1' = scale2ToCenter y1 centerY
    y2' = scale2ToCenter y2 centerY
scale2 text@(Text _ _) = text
scale2 (Clipped point w h picture) = Clipped point (w/2.0) (h/2.0) picture


getText :: Shape -> Maybe String
getText (Text _ text) = Just text
getText _ = Nothing


area :: Shape -> Number
area (Circle c r) = r * r * Math.pi
area (Rectangle c w h) = w * h
area (Clipped point w h picture) = w * h
area _ = 0.0

