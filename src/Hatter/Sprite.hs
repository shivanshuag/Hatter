module Hatter.Sprite where

import Hatter.AssetStore
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Foreign.C.String
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)

data Sprite = Sprite{file :: String, width :: Int, height :: Int}

loadTexture :: SDL.Renderer -> String -> AssetStore ->(IO SDL.Texture, AssetStore)
loadTexture renderer path assetStore = case lookup path $ images assetStore of
    Just texture -> (texture, assetStore)
    Nothing -> let texture = loadImageFromFile renderer path 
                   in (texture, updateImageStore assetStore path texture)

loadImageFromFile :: SDL.Renderer -> String -> IO SDL.Texture
loadImageFromFile renderer path = Image.imgLoadTexture renderer path >>=either throwSDLError return
                                        

throwSDLError :: String -> IO a
throwSDLError message = do
        errorString <- SDL.getError >>= peekCString
        fail (message ++ "SDL_Error:" ++ errorString)
