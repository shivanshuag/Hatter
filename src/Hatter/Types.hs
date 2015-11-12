module Hatter.Types where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types
import Data.Map as Map
import Linear

type AssetStore = Map String (IO SDL.Texture)

data Sprite = Sprite{file :: String, width :: Int, height :: Int}

type MouseEvent = SDL.Event

data BoundingBox = Circle (V2 Double) Double | Rectangle (V2 Double) (V2 Double) | None
