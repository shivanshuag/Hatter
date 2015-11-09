module Hatter.AssetStore where

import Data.Map
import qualified SDL
import Data.Lens.Common

data AssetStore = AssetStore {images :: Map String (IO SDL.Texture)
--                             , sounds ::
                             }

updateImageStore :: AssetStore -> path -> IO SDL.Texture -> AssetStore
updateImageStore oldstore path texture = 
        let newImageStore = Map.insert path texture $ images oldstore
            in setL images newImageStore oldstore 
