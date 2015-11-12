module Hatter.Sprite where

import Hatter.Types
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Utils
import Control.Monad.IO.Class (liftIO)
import Linear
import Data.Lens.Common
import Data.Map as Map hiding (foldl)
import System.FilePath.Find

loadTexture :: SDL.Renderer -> AssetStore -> String -> AssetStore 
loadTexture renderer assetStore path = 
        let texture = loadImageFromFile renderer path 
                   in updateImageStore assetStore path texture

loadImageFromFile :: SDL.Renderer -> String -> IO SDL.Texture
loadImageFromFile renderer path = Image.imgLoadTexture renderer path >>=either throwSDLError return
                                        
throwSDLError :: String -> IO a
throwSDLError message = do
        errorString <- SDL.getError >>= peekCString
        fail (message ++ "SDL_Error:" ++ errorString)

renderSprite :: SDL.Renderer -> Sprite -> V2 Double -> SDL.Texture -> IO ()
renderSprite renderer sprite (V2 x y) texture = do
        let rect =
                SDL.Rect { rectX = fromIntegral (round x), rectY = fromIntegral (round y), rectW = fromIntegral $ width sprite, rectH = fromIntegral $ height sprite}
        with rect $ SDL.renderCopy renderer texture nullPtr
        return () 


updateImageStore :: AssetStore -> String -> IO SDL.Texture -> AssetStore
updateImageStore oldstore path texture = Map.insert path texture oldstore

buildAssetStore :: SDL.Renderer -> FilePath -> IO AssetStore
buildAssetStore renderer path = do
        let allowedExtensionsPng = extension ==? ".PNG" ||? extension ==? ".png" 
        let allowedExtensionsJpg = extension ==? ".jpg" ||? extension ==? ".JPG"
        let allowedExtensionsJpeg = extension ==? ".jpeg" ||? extension ==? ".JPEG"
        files <- find always (allowedExtensionsPng ||? allowedExtensionsJpg ||? allowedExtensionsJpeg) path
        let assetStore = Map.fromList []
        return $ foldl (loadTexture renderer) assetStore files

