module Hatter where

import Control.Wire as Wire
import Prelude hiding ((.), id, null, filter)
import Data.Map as Map hiding (foldl)
import Data.Set as Set hiding (foldl)
import Linear.V2
import Data.Text hiding (foldl)
import Hatter.Types
import Hatter.Sprite
import Hatter.Draw
import Hatter.BoundingBox
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Event
import Graphics.UI.SDL.Types
import Data.Bits
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.C.String
import Foreign.C.Types
import Data.Time (NominalDiffTime)


-- implement Sound
-- data GameSound = GameSound

-- render function should render a GameObject on the screen
render :: [GameObject] -> SDL.Renderer -> AssetStore -> IO ()
render (object:others) renderer assetStore = case gameGraphic object of
    Image sprite ->
        let maybeTexture = Map.lookup (file sprite) assetStore
            in
            case maybeTexture of
            Just iotexture -> do
                print $ position object
                texture <- iotexture
                renderSprite renderer sprite (position object) texture
                render others renderer assetStore
            Nothing -> render others renderer assetStore


    --Geometry geom -> ()

render [] _ _ = return ()


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

createInitialState :: Map String GameObject -> b -> GameState b
createInitialState  objects externalstate= GameState {keyEvents=Set.empty
                                    ,mouseEvents=[]
                                    ,sdlEvents=[]
                                    ,collisions=Map.fromList []
                                    ,events=Map.fromList []
                                    ,gameObjects=objects
                                    ,extraState=externalstate
                                    ,dt=0
                                    }
--}
--
intersecting :: GameObject -> GameObject -> Bool
intersecting o1 o2 = Hatter.BoundingBox.intersecting (Hatter.BoundingBox.translate (boundingBox o1) (position o1)) (translate (boundingBox o2) (position o2))


pollEvents :: IO (Maybe SDL.Event)
pollEvents = alloca $ \pointer -> do
    status <- pollEvent pointer
    if status==1 then maybePeek peek pointer else return Nothing

getEventsHelper :: IO [SDL.Event]
getEventsHelper = do
  maybeevent <- pollEvents
  case maybeevent of
    Nothing -> return []
    Just event -> do
      events <- getEventsHelper
      return $ event:events

-- Used for getting the events from SDL after each frame.
-- Separates all the events into Keypress, MousEvent and other SDL events
getEvents :: Set KeyPress -> IO (Set KeyPress, [MouseEvent], [SDL.Event])
getEvents keyPress = do
  events <- getEventsHelper
  return $ parseEvents (keyPress, [], []) events

-- helper function used by getEvents to separate events
parseEvents :: (Set KeyPress, [MouseEvent], [SDL.Event]) -> [SDL.Event] -> (Set KeyPress, [MouseEvent], [SDL.Event])
parseEvents e [] = e
parseEvents ((keyPress, mouseEvents, otherEvents)) (event:others) = case event of
  KeyboardEvent{}->
    parseEvents
      (processKeyEvent keyPress event, mouseEvents, otherEvents) others
  SDL.MouseButtonEvent{} ->
    parseEvents
      (keyPress, event:mouseEvents, otherEvents) others
  SDL.MouseMotionEvent{} ->
    parseEvents
      (keyPress, event:mouseEvents, otherEvents) others
  SDL.MouseWheelEvent{} ->
    parseEvents
      (keyPress, event:mouseEvents, otherEvents) others
  _ ->
    parseEvents (keyPress, mouseEvents, event:otherEvents) others

processKeyEvent :: Set KeyPress -> SDL.Event -> Set KeyPress
processKeyEvent keyPress (SDL.KeyboardEvent eventType _ _ _ _ (Keysym _ keycode _)) |
  eventType == SDL.SDL_KEYUP = Set.delete keycode keyPress
  | eventType == SDL.SDL_KEYDOWN = Set.insert keycode keyPress
  | otherwise = keyPress

updateState :: GameState b -> Map String GameObject -> Double -> IO (GameState b)
updateState oldstate newObjects newdt= do
        (keys, mouse, other) <- getEvents $ keyEvents oldstate
        --TODO: check for  collisionEvents
        --TODO: check for user event checkers
        let newstate = GameState {keyEvents=keys
                             ,mouseEvents=mouse
                             ,sdlEvents=other
                             ,collisions=collisions oldstate
                             ,events=events oldstate
                             ,gameObjects=newObjects
                             ,extraState=extraState oldstate
                             ,dt=newdt
                             }
        return newstate



--TODO: Remove external state from game definition and write logic to
--handle external state.
-- Called by the user with the game definition and the canvas to run the game
run:: GameDefinition (Timed NominalDiffTime()) e b -> Canvas -> GameState b -> IO ()
run definition (Canvas width height title) initialState = do
    ctitle <- newCString title
    SDL.init $ foldl (.|.) 0 [SDL.SDL_INIT_VIDEO]
    window <- SDL.createWindow ctitle SDL.SDL_WINDOWPOS_UNDEFINED SDL.SDL_WINDOWPOS_UNDEFINED (fromIntegral width) (fromIntegral height) SDL.SDL_WINDOW_SHOWN
    renderer <- SDL.createRenderer window (-1) $ foldl (.|.) 0 [SDL.SDL_RENDERER_ACCELERATED]

    gameLoop renderer definition initialState (countSession_ $ 1 / frameRate definition ) $ buildAssetStore renderer $ assetDir definition


-- The Game Loop. Will run each of the wire, extract new list of game object, calculate new state and rerun the state
gameLoop :: (Real t, HasTime t s) => SDL.Renderer -> GameDefinition s e a -> GameState a -> Session IO s -> IO AssetStore -> IO ()
gameLoop renderer definition state session assetStore = do
  SDL.renderClear renderer
  (keyPress, mouseEvents, otherEvents) <- getEvents $ keyEvents state
  (ds, newSession) <- liftIO $ Wire.stepSession session
  (objects, newWire) <- Wire.stepWire (gameWire definition) ds (Right state)
  -- create a new state here
  -- Provide a way for the user to modify extraState
  -- constructNewState :: GameState -> GameState
  -- render the objects here
  store <- assetStore
  newstate <- case objects of
                  Right objects -> do
                      render (Map.elems objects) renderer store
                      updateState state objects $ realToFrac $ dtime ds
                  Left error -> updateState state (Map.fromList []) $ realToFrac $ dtime ds
  SDL.renderPresent renderer
  gameLoop renderer definition newstate newSession assetStore
