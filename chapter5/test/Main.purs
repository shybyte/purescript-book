module Test.Main where

import Prelude
import Control.Monad.Eff.Console

import Data.Picture
import Solution

circle :: Shape
circle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

rectangle :: Shape
rectangle = Rectangle (Point { x: 10.0, y: 10.0 }) 10.0 10.0

picture :: Picture
picture = [circle, rectangle]

main = do

  print $ factorial 5
  print $ binomialCoefficients 5 0
  print $ binomialCoefficients 6 6
  print $ binomialCoefficients 5 3
  print $ sameCity
    {address: {city: "Berlin", street: "Bla"}, name: "Marco"}
    {address: {city: "Jena", street: "Bli"}, name: "Lars"}
  print $ sameCity
    {address: {city: "Jena", street: "Blo"}, name: "Lars"}
    {address: {city: "Jena", street: "Bli"}, name: "Sven"}
  print $ fromSingleton "default" []
  print $ fromSingleton "default" ["Supi"]
  print $ fromSingleton 0 [1,2]
  print $ showShape $ scale2 $ Line (Point {x: 10.0, y: 20.0}) (Point {x: 20.0, y: 30.0})
  print $ getText $ Line (Point {x: 10.0, y: 20.0}) (Point {x: 20.0, y: 30.0})
  print $ getText $ Text (Point {x: 10.0, y: 20.0}) "Text of Text"
  log (showBounds (bounds picture))
  print $ showBounds $ shapeBounds $ Clipped (Point{x:0.0,y:0.0}) 10.0 10.0 picture
  print $ showBounds $ shapeBounds $ Clipped (Point{x:0.0,y:0.0}) 50.0 50.0 picture