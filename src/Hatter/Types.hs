module Hatter.Types where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types
import Data.Map as Map
import Data.Set as Set
import Linear
import Control.Wire

type AssetStore = Map String (IO SDL.Texture)

data Sprite = Sprite{file :: String, width :: Int, height :: Int}

type MouseEvent = SDL.Event

data BoundingBox = Circle (V2 Double) Double | Rectangle (V2 Double) (V2 Double) | None

-- | The entire window where the game happens. Take widht, height and the name of the window.
data Canvas = Canvas Int Int String

-- | type alias for keycodes given by SDL in keyboardevent.
-- Look at https://hackage.haskell.org/package/sdl2-2.1.0/docs/SDL-Input-Keyboard-Codes.html#v:Keycode for details on how to pattern match them
type KeyPress = SDL.Keycode

data Draw = Draw{line :: V2 Int, point :: V2 Int}

-- | A graphic can either be a sprite or a geometry. Graphic has an associated render function.
data GameGraphic =  Image Sprite | Geometry Draw

-- | Game Object is the smallest renderable entity of the game.
data GameObject = GameObject{oid:: String
                             -- ^ An id to uniquely identify the object
                            ,position :: V2 Double
                             -- ^ Position where this game object is rendered
                            ,boundingBox :: BoundingBox
                             -- ^ Bounding Box for detecting collisions to this object
                            ,gameGraphic :: GameGraphic
                             -- ^ A graphic associated to this object. This grapahic will be renderd at the position of this object.
                            }

-- Game Computation is the information calculated from the last frame of the game
-- Library can modify only the extraState part of this type.
data GameState b = GameState {keyEvents :: Set KeyPress
                              -- ^ Set containing the keycodes of keys pressed in the current frame.
                              -- ^ Pattern match the corresponding key from https://hackage.haskell.org/package/sdl2-2.1.0/docs/SDL-Input-Keyboard-Codes.html#v:Keycode
                             ,mouseEvents ::  [MouseEvent]
                              -- ^ List of mouse events that occured after previous frame
                             ,sdlEvents :: [SDL.Event]
                              -- ^ List of SDL events other than mouse and keyboard event that occured after the previous frame
                              -- ^ Lookup https://hackage.haskell.org/package/sdl2-2.1.0/docs/SDL-Event.html#t:Event for the list of all events
                             ,collisions :: Map String GameObject
                             ,events :: Map String ()
                             ,gameObjects :: Map String GameObject
                             -- ^ gameobjects obtained by running the wire in the previous frame. These are rendered on the screen
                             ,extraState :: b
                             -- ^ the externalState supplied by and modified by the user
                             ,dt :: Double
                             }

-- Definition of the game.
data GameDefinition s e b = GameDefinition {gameWire :: Wire s e IO (GameState b) (Map String GameObject)
                                              -- ^ Wire has all the logic of the game. The wire takes a GameState and returns a List of GameObjects which are rendered
                                             ,eventCheckers :: Map String (GameState b -> ())
                                             -- ^ used for interaction between GameObjects. These are checked in each frame.
                                             ,frameRate :: NominalDiffTime
                                             -- ^ frame-rate of the game
                                             ,externalState :: b
                                             -- ^ State if the game which is supplied by and can be modified by the user
                                             ,assetDir :: FilePath
                                             }
