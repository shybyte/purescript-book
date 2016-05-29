module Data.Picture where

import Data.Foldable (foldMap, foldl)
import Data.Maybe (Maybe(Nothing, Just))
import Prelude ((*), negate, (/), (+), (-), show, (++), map)

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x = x, y = y }) =
  "(" ++ show x ++ ", " ++ show y ++ ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture Point Number Number

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " ++ showPoint c ++ ", radius: " ++ show r ++ "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " ++ showPoint c ++ ", width: " ++ show w ++ ", height: " ++ show h ++ "]"
showShape (Line start end) =
  "Line [start: " ++ showPoint start ++ ", end: " ++ showPoint end ++ "]"
showShape (Text loc text) =
  "Text [location: " ++ showPoint loc ++ ", text: " ++ show text ++ "]"
showShape (Clipped picture c w h) =
  "Clipped [center: " ++ showPoint c ++
  ", width: " ++ show w ++
  ", height: " ++ show h ++
  ", picture: " ++ (foldMap showShape picture) ++
  "]"

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " ++ show b.top ++
  ", left: "      ++ show b.left ++
  ", bottom: "    ++ show b.bottom ++
  ", right: "     ++ show b.right ++
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x = x, y = y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x = x, y = y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x = x, y = y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }

shapeBounds (Clipped picture center w h) =
  bounds picture /\ shapeBounds (Rectangle center w h)

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

infix 4 union as \/

intersection :: Bounds -> Bounds -> Bounds
intersection (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

infix 4 intersection as /\

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = shapeBounds shape \/ b


-- 1. Exercise
-- Construct a value of type Shape which represents a circle centered at the origin with radius 10.0.

centeredCircle :: Shape
centeredCircle = Circle (Point {x: 0.0, y: 0.0}) 10.0


-- 2. Exercise
-- Write a function from Shapes to Shapes, which scales its argument by a factor of 2.0,
-- center the origin.

scale ::  Shape -> Shape
scale (Circle center r) = Circle center (2.0 * r)
scale (Rectangle center w h) = Rectangle center (2.0 * w) (2.0 * h)
scale (Line (Point p1) (Point p2)) = Line (Point p1') (Point p2')
  where
  centerX = mean p1.x p2.x
  centerY = mean p1.y p2.y
  p1' = { x: 2.0 * (p1.x - centerX) + centerX,  y: 2.0 * (p1.y - centerY) + centerY}
  p2' = { x: 2.0 * (p2.x - centerX) + centerX,  y: 2.0 * (p2.y - centerY) + centerY}
scale text = text


mean :: Number -> Number -> Number
mean x y = (x + y) / 2.0


-- 3. Exercise
-- Write a function which extracts the text from a Shape.
-- It should return Maybe String, and use the Nothing constructor
-- if the input is not constructed using Text.

extractText :: Shape -> Maybe String
extractText (Text _ text) = Just text
extractText _ = Nothing


-- 5.16

-- 1. Exercise
-- Extend the vector graphics library with a new operation area
-- which computes the area of a Shape.
-- For the purposes of this exercise, the area of a piece of text is assumed to be zero.

area ::  Shape -> Number
area (Circle _ r) = Math.pi * r * r
area (Rectangle _ w h) = w * h
area (Clipped picture _ w h) = w * h
area (Line _ _) = 0.0
area (Text _ _) = 0.0
