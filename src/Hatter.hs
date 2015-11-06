module Hatter where

import Control.Wire
import Prelude hiding ((.), id, null, filter)
import Data.Map
import Data.Set
import Linear.V2
import Hatter.Sprite
import Hatter.Draw
import Hatter.BoundingBox
import Hatter.AssetStore
import qualified SDL

-- Game object should be a wire from GameState to some vector-graphic/sprite/sound. A figure will contain the coordinates of its
-- TODO: Look at how to represent a vector graphic
data GameObject = VectorGraphic{ id:: String
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
render :: GameObject -> SDL.Renderer -> AssetStore -> IO()
render object renderer assetStore = case gameGraphic object of
    Image sprite -> ()
    Geometry geom -> ()


-- Game Computation is the information calculated from the last frame of the game
-- Library user never have to modify change this.
data GameState b = GameState { keyInput :: Set KeyInput
--                           , mouseInput ::
                            , collisions :: Map String GameObject
                            , events :: Map String ()
                            , gameObjects :: [GameObject]
                            , externalState :: b
                            }

-- Definition of the game. Can add more wires or events during the game runtime.
data GameDefinition b = GameDefinition { gameWire :: Wire () () Identity (GameState b) GameObject
                                       , eventCheckers :: Map String (GameState b -> ())
                                       }

run:: GameDefinition b -> Canvas -> IO ()
run (Canvas width height title) definition = do
    SDL.initialize [SDL.InitEverything]
    let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 width height }
    window <- SDL.createWindow title winConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    let assetStore = AssetStore Data.Map.fromList([])
    gameLoop renderer definition clockSession_ assetStore

-- The Game Loop. Will run each of the wire, extract new list of game object, calculate new state and rerun the state
-- gameLoop :: SDL.Renderer -> GameState -> Session IO s -> IO b

-- constructNewState :: GameState -> GameState


-- User will create wires from GameState b to GameObject and give them to the run function
