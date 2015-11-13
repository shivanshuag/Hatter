module Hatter.BoundingBox where

import           Linear
import Hatter.Types

intersects :: BoundingBox -> BoundingBox -> Bool
intersects (Circle center1 radius1) (Circle center2 radius2) =  quadrance (center1 - center2) < (radius1 + radius2)*(radius1+radius2)
intersects (Rectangle (V2 x11 y11) (V2 x12 y12)) (Rectangle (V2 x21 y21) (V2 x22 y22))
  = intersect1D x11 x12 x21 x22 && intersect1D y11 y12 y21 y22
intersects (Circle center radius) (Rectangle p1 p2) = False
intersects (Rectangle p1 p2) (Circle center radius) = intersects (Circle center radius) (Rectangle p1 p2)

intersect1D :: Double -> Double -> Double -> Double -> Bool
intersect1D x11 x12 x21 x22 = not((x12 < x21) || (x11 > x22))

translate :: BoundingBox -> V2 Double -> BoundingBox
translate (Circle center radius) position = Circle (center + position) radius
translate (Rectangle p1 p2) position = Rectangle (p1+position) (p2+position)
translate None _ = None
