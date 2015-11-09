module Hatter.AssetStore where

import Hatter.Texture
import Data.Map
import qualified SDL
import Data.Lens.Common
import System.FilePath.Find

data AssetStore = AssetStore {images :: Map String (IO SDL.Texture)
--                             , sounds ::
                             }

updateImageStore :: AssetStore -> path -> IO SDL.Texture -> AssetStore
updateImageStore oldstore path texture = 
        let newImageStore = Map.insert path texture $ images oldstore
            in setL images newImageStore oldstore

buildAssetStore :: SDL.Renderer -> FilePath -> IO AssetStore
buildAssetStore path = do
        let allowedExtensionsPng = extension ==? ".PNG" ||? extension ==? ".png" 
        let allowedExtensionsJpg = extension ==? ".jpg" ||? extension ==? ".JPG"
        let allowedExtensionsJpeg = extension ==? ".jpeg" ||? extension ==? ".JPEG"
        files <- find always (allowedExtensionsPng ||? allowedExtensionsJpg ||? allowedExtensionsJpeg) path
        let assetStore = Map.fromList []
        return $ foldl (loadTexture renderer) assetStore files

