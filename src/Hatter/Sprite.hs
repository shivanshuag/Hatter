module Hatter.Sprite where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Foreign.C.String

data Sprite = Sprite{file :: String, width :: Int, height :: Int}

loadTexture :: SDL.Renderer -> String -> IO SDL.Texture
loadTexture renderer path = Image.imgLoadTexture renderer path >>=either throwSDLError return

throwSDLError :: String -> IO a
throwSDLError message = do
        errorString <- SDL.getError >>= peekCString
        fail (message ++ "SDL_Error:" ++ errorString)
