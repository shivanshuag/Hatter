module Hatter.Game where

import Control.Wire
import Prelude hiding ((.), id, null, filter)
import Data.Map
import Data.Set
import Linear.V2
import Hatter.Sprite
import Hatter.Draw
import Hatter.BoundingBox
import qualified SDL

-- Game object should be a wire from GameState to some vector-graphic/sprite/sound. A figure will contain the coordinates of its
-- TODO: Look at how to represent a vector graphic
data GameObject = VectorGraphic{id:: String
                               , position :: V2 Int
                               , boundingbox :: BoundingBox
                               , gameGraphic :: GameGraphic
                               }

data Canvas = Canvas Int Int String

data GameGraphic =  Image Sprite | Geometry Draw

type KeyInput = SDL.Keysym
-- implement Sound
-- data GameSound = GameSound

-- render function should render a GameObject on the screen
-- render :: GameGraphic -> Ren -> IO()


-- Game State should contain key inputs, mouse inputs, collisions, time delta
data GameState = GameState { keyInput :: Set KeyInput
--                          , mouseInput ::
                           , collisions :: Map String GameObject
                           , gameObjects :: [GameObject]
                           , gameWires :: [Wire () () Identity GameState GameObject]
--                           , deltatime::
                           }

run:: GameState -> Canvas -> IO ()
run (Canvas width height title) state = do
    SDL.initialize [SDL.InitEverything]
    let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 width height }
    window <- SDL.createWindow title winConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    gameLoop renderer state clockSession_

-- The Game Loop. Will run each of the wire, extract new list of game object, calculate new state and rerun the state
-- gameLoop :: SDL.Renderer -> GameState -> Session IO s -> IO b
