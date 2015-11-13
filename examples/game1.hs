{-# LANGUAGE Arrows #-}
import Hatter
import Hatter.Types
import Hatter.Wires
import Control.Wire as Wire hiding (id)
import Control.Arrow
import Control.Applicative
import FRP.Netwire hiding (id)
import Prelude hiding ((.), null, filter)
import Data.Map as Map hiding (foldl)
import Data.Set as Set hiding (foldl)
import Linear hiding (trace)
import Debug.Trace
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Event
import Graphics.UI.SDL.Types

--Extra Game State in this game will be number of rectangles on the screen.
main = do
    print $ show $ Prelude.map oid $ playerObject:listObstacles
    run myGameDefinition canvas initialGameState


objectName :: String
objectName = assetDirPath ++ "square.png"

playerName :: String
playerName = assetDirPath ++ "player.png"

--Generate inital game state.
initialGameState :: GameState Integer 
initialGameState = createInitialState (Map.fromList $ Prelude.map (\obj -> (oid obj,obj)) $ playerObject:listObstacles) 0


squareObject :: String -> V2 Double -> GameObject
squareObject objid pos = GameObject objid pos Hatter.Types.None (Image $ Hatter.Types.Sprite objectName 10 120)

playerObject :: GameObject
playerObject = GameObject "pl" (V2 300 500) Hatter.Types.None (Image $ Hatter.Types.Sprite playerName 50 100)

initialPos :: V2 Double
initialPos = V2 (1000+10*180) (600-120)

obstacles :: Double -> [GameObject]
obstacles n | n == 0 = [] | otherwise = squareObject (show n) (initialPos - (V2 (n*180.0) 0)):obstacles (n-1)

listObstacles :: [GameObject]
listObstacles = obstacles 10

--Ggenerate Wires
myPositionResetFunction :: GameState Integer -> V2 Double-> V2 Double
myPositionResetFunction state (V2 x y) | x < negate 10 = V2 (1000+10*180) y | otherwise = V2 x y

myVelocityResetFunction :: GameState Integer -> V2 Double-> V2 Double
myVelocityResetFunction state (V2 x y) | x < negate 1000 = V2 0 y | otherwise = V2 x y

extractObject :: Maybe GameObject -> GameObject
extractObject (Just object) = object
extractObject Nothing = head listObstacles 

extractPlayer :: Maybe GameObject -> GameObject
extractPlayer (Just object) = object
extractPlayer Nothing = playerObject

check :: V2 Double -> Bool
check (V2 _ y) | y < 500 = True | otherwise = False 

playerAccel :: (Real t, HasTime t s) => AccelerationWire s e Integer
playerAccel = proc state -> do
  let object = extractPlayer $ Map.lookup "pl" $ gameObjects state
  let inair = check $ position object
  let jump = Set.member SDLK_SPACE $ keyEvents  state
  returnA -< if inair || jump then V2 0 $ 5 else V2 0 $ negate 10

correctVelocity :: CorrectionFunction Integer
correctVelocity state vel =  
  let object = extractPlayer $ Map.lookup "pl" $ gameObjects state
      inair = check $ position object
      jump = Set.member SDLK_SPACE $ keyEvents  state
  in 
     if not inair && jump then V2 0 (negate 40) else if inair then vel else V2 0 0

correctPosition :: CorrectionFunction Integer
correctPosition state (V2 x y) = if y >= 500 then V2 x 500 else V2 x y 

playerGameWire :: (Real t, HasTime t s) => Wire s e IO (GameState Integer) GameObject
playerGameWire = proc state -> do
  let object = extractPlayer $ Map.lookup "pl" $ gameObjects state
  newposition <- positionWire correctPosition (V2 300 500) (velocityWire correctVelocity (V2 0 0)) playerAccel -< state 
  let newobject = GameObject {oid=oid object, position=newposition, boundingBox=boundingBox object, gameGraphic=gameGraphic object}
  returnA -< newobject

myGameWire :: (Real t, HasTime t s) => V2 Double -> String -> Wire s e IO (GameState Integer) GameObject
myGameWire pos objid =  proc state -> do
  let object = extractObject $ Map.lookup objid $ gameObjects state 
  newposition <- positionWire myPositionResetFunction pos (velocityWire myVelocityResetFunction $ V2 (negate 10) 0) (constantAcceleration $ V2 (negate 0.2) 0) -< state
  let newobject = GameObject {oid=oid object, position=newposition, boundingBox=boundingBox object, gameGraphic=gameGraphic object}
  returnA -< newobject
    
generateWire ::  (Real t, HasTime t s) => [GameObject] -> Wire s e IO (GameState Integer) (Map String GameObject)
generateWire lstobstacles= combineWire playerGameWire $ foldl (\wire object-> combineWire (myGameWire (position object) $ oid object) wire) emptyWire lstobstacles
myEventCheckers :: Map String (GameState Integer -> ())
myEventCheckers = Map.fromList []

assetDirPath :: String
assetDirPath = "/Users/apple/Documents/Academics/sem9/FP/Hatter/examples/assets/"

canvas :: Canvas
canvas = Canvas 1000 600 "Square"

myGameDefinition = GameDefinition { gameWire=generateWire listObstacles
                                   ,eventCheckers=myEventCheckers
                                   ,frameRate=20
                                   ,externalState=0
                                   ,assetDir=assetDirPath
                                   }

