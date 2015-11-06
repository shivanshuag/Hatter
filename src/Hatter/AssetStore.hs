module Hatter.AssetStore where

import Data.Map
import qualified SDL

data AssetStore = AssetStore {images :: Map String SDL.Texture
--                             , sounds ::
                             }
