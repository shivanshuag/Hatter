module Hatter.Types where

import Graphics.UI.SDL as SDL
import Data.Map as Map

type AssetStore = Map String (IO SDL.Texture)

data Sprite = Sprite{file :: String, width :: Int, height :: Int}
