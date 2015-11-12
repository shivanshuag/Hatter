module Hatter.BoundingBox where

import           Linear
import Hatter.Types
-- Implement BoundingBox and Collisions Bounding box used for collision of a game Object
-- The coordinates of the bounding box are given relative to the gameObject i.e. by taking the position of the gameObject as (0,0)

intersecting :: BoundingBox -> BoundingBox -> Bool
intersecting (Circle center1 radius1) (Circle center2 radius2) =  quadrance (center1 - center2) < (radius1 + radius2)*(radius1+radius2)
-- intersecting (Rectangle p11 p12) (Rectangle p21 p22) = 

translate :: BoundingBox -> V2 Double -> BoundingBox
translate (Circle center radius) position = Circle (center + position) radius
translate (Rectangle p1 p2) position = Rectangle (p1+position) (p2+position)
translate None _ = None
