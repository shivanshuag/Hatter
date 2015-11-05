module Hatter.BoundingBox where

import Linear
-- Implement BoundingBox and Collisions Bounding box used for collision of a game Object
data BoundingBox = BoundingCircle (V2 Double) Double | BoundingRectangle (V2 Double) (V2 Double) | None
