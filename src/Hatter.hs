module Hatter where

import Control.Wire as Wire
import Prelude hiding ((.), id, null, filter)
import Data.Map as Map
import Data.Set as Set
import Linear.V2
import Data.Text
import Hatter.Sprite
import Hatter.Draw
import Hatter.BoundingBox
import Hatter.AssetStore
import Control.Monad.IO.Class (liftIO)
import qualified SDL

-- | Game Object is the smallest renderable entity of the game.
data GameObject = GameObject{id:: String
                             -- ^ An id to uniquely identify the object
                            ,position :: V2 Int
                             -- ^ Position where this game object is rendered
                            ,boundingbox :: BoundingBox
                             -- ^ Bounding Box for detecting collisions to this object
                            ,gameGraphic :: GameGraphic
                             -- ^ A graphic associated to this object. This grapahic will be renderd at the position of this object.
                            }

-- | The entire window where the game happens. Take widht, height and the name of the window.
data Canvas = Canvas Int Int String

-- | A graphic can either be a sprite or a geometry. Graphic has an associated render function.
data GameGraphic =  Image Sprite | Geometry Draw

-- | Data type for mouse events. Look at https://hackage.haskell.org/package/sdl2-2.1.0/docs/SDL-Event.html#t:Event for more details
data MouseEvent = MouseMotionEvent SDL.MouseMotionEventData | MouseButtonEvent SDL.MouseButtonEventData | MouseWheelEvent SDL.MouseWheelEventData

-- | type alias for keycodes given by SDL in keyboardevent.
-- Look at https://hackage.haskell.org/package/sdl2-2.1.0/docs/SDL-Input-Keyboard-Codes.html#v:Keycode for details on how to pattern match them
type KeyPress = SDL.Keycode

-- implement Sound
-- data GameSound = GameSound

-- render function should render a GameObject on the screen
render :: [GameObject] -> SDL.Renderer -> AssetStore -> IO ()
render (object:others) renderer assetStore = case gameGraphic object of
    Image sprite -> ()
    Geometry geom -> ()


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
                             ,gameObjects :: [GameObject]
                             -- ^ gameobjects obtained by running the wire in the previous frame. These are rendered on the screen
                             ,extraState :: b
                             -- ^ the externalState supplied by and modified by the user
                             }

-- Definition of the game.
data GameDefinition s e b = GameDefinition {gameWire :: Wire s e IO (GameState b) [GameObject]
                                              -- ^ Wire has all the logic of the game. The wire takes a GameState and returns a List of GameObjects which are rendered
                                             ,eventCheckers :: Map String (GameState b -> ())
                                             -- ^ used for interaction between GameObjects. These are checked in each frame.
                                             ,frameRate :: NominalDiffTime
                                             -- ^ frame-rate of the game
                                             ,externalState :: b
                                             -- ^ State if the game which is supplied by and can be modified by the user
                                             }

initialState ::  GameDefinition s e b-> GameState b
initialState definition = GameState {keyEvents=Set.empty
                                    ,mouseEvents=[]
                                    ,sdlEvents=[]
                                    ,collisions=Map.fromList []
                                    ,events=Map.fromList []
                                    ,gameObjects=[]
                                    ,extraState=externalState definition
                                    }

-- Used for getting the events from SDL after each frame.
-- Separates all the events into Keypress, MousEvent and other SDL events
getEvents :: Set KeyPress -> IO (Set KeyPress, [MouseEvent], [SDL.Event])
getEvents keyPress = do
  events <- SDL.pollEvents
  return $ parseEvents (keyPress, [], []) events

-- helper function used by getEvents to separate events
parseEvents :: (Set KeyPress, [MouseEvent], [SDL.Event]) -> [SDL.Event] -> (Set KeyPress, [MouseEvent], [SDL.Event])
parseEvents e [] = e
parseEvents ((keyPress, mouseEvents, otherEvents)) (event:others) = case SDL.eventPayload event of
  SDL.KeyboardEvent eventData ->
    parseEvents
      (processKeyEvent keyPress eventData, mouseEvents, otherEvents) others
  SDL.MouseButtonEvent eventData ->
    parseEvents
      (keyPress, MouseButtonEvent eventData:mouseEvents, otherEvents) others
  SDL.MouseMotionEvent eventData ->
    parseEvents
      (keyPress, MouseMotionEvent eventData:mouseEvents, otherEvents) others
  SDL.MouseWheelEvent eventData ->
    parseEvents
      (keyPress, MouseWheelEvent eventData:mouseEvents, otherEvents) others
  _ ->
    parseEvents (keyPress, mouseEvents, event:otherEvents) others

processKeyEvent :: Set KeyPress -> SDL.KeyboardEventData -> Set KeyPress
processKeyEvent keyPress eventData = case SDL.keyboardEventKeyMotion eventData of
  SDL.Released -> Set.delete (getKeyPressfromEvent eventData) keyPress
  SDL.Pressed -> Set.insert (getKeyPressfromEvent eventData) keyPress

getKeyPressfromEvent :: SDL.KeyboardEventData -> KeyPress
getKeyPressfromEvent eventData = SDL.keysymKeycode $ SDL.keyboardEventKeysym eventData

-- Called by the user with the game definition and the canvas to run the game
run:: GameDefinition (Timed NominalDiffTime()) e b -> Canvas -> IO ()
run definition (Canvas width height title) = do
    SDL.initialize [SDL.InitEverything]
    let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 (fromIntegral width) (fromIntegral height) }
    window <- SDL.createWindow (pack title) winConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    let assetStore = AssetStore $ Map.fromList []
    let state = initialState definition
    gameLoop renderer definition state (countSession_ $ 1 / frameRate definition ) assetStore


-- The Game Loop. Will run each of the wire, extract new list of game object, calculate new state and rerun the state
gameLoop :: SDL.Renderer -> GameDefinition s e a -> GameState a -> Session IO s -> AssetStore -> IO ()
gameLoop renderer definition state session assetStore = do
  (keyPress, mouseEvents, otherEvents) <- getEvents $ keyEvents state
  (ds, newSession) <- liftIO $ Wire.stepSession session
  (objects, newWire) <- Wire.stepWire (gameWire definition) ds (Right state)
  -- create a new state here
  -- Provide a way for the user to modify extraState
  -- constructNewState :: GameState -> GameState
  -- render the objects here
  case objects of
    Right list -> render list renderer assetStore
  gameLoop renderer definition state newSession assetStore
