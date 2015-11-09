module Hatter.Sprite where

import Hatter.AssetStore
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Foreign.C.String
import Foreign.Ptr
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)

data Sprite = Sprite{file :: String, width :: Int, height :: Int}

loadTexture :: SDL.Renderer -> AssetStore -> String -> AssetStore 
loadTexture renderer assetStore path = 
        let texture = loadImageFromFile renderer path 
                   in updateImageStore assetStore path texture

loadImageFromFile renderer path = Image.imgLoadTexture renderer path >>=either throwSDLError return
                                        
throwSDLError :: String -> IO a
throwSDLError message = do
        errorString <- SDL.getError >>= peekCString
        fail (message ++ "SDL_Error:" ++ errorString)

renderSprite :: SDL.Renderer -> Sprite -> V2 Int -> SDL.Texture -> IO ()
renderSprite renderer sprite (V2 x y) texture = do
        let rect =
                SDL.Rect { rectX = x, rextY = y, rectW = width sprite, rectH = height sprite}
        SDL.renderCopy renderer texture nullPtr rect
        return () 
