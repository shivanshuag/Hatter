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
import Linear hiding (trace)
import Debug.Trace

--Extra Game State in this game will be number of rectangles on the screen.
main = do
    print $ show $ Prelude.map oid listObstacles
    run myGameDefinition canvas initialGameState


objectName :: String
objectName = assetDirPath ++ "square.png"

--Generate inital game state.
initialGameState :: GameState Integer 
initialGameState = createInitialState (Map.fromList $ Prelude.map (\obj -> (oid obj,obj)) listObstacles) 0

squareObject :: String -> V2 Double -> GameObject
squareObject objid pos = GameObject objid pos Hatter.Types.None (Image $ Hatter.Types.Sprite objectName 10 200)

initialPos :: V2 Double
initialPos = V2 1200 500

obstacles :: Double -> [GameObject]
obstacles n | n == 0 = [] | otherwise = squareObject (show n) (initialPos - (V2 (n*30.0) 0)):obstacles (n-1)

listObstacles :: [GameObject]
listObstacles = obstacles 10

--Ggenerate Wires
myPositionResetFunction :: GameState Integer -> V2 Double-> V2 Double
myPositionResetFunction state (V2 x y) | x < negate 10 = V2 1000 y | otherwise = V2 x y

extractObject :: Maybe GameObject -> GameObject
extractObject (Just object) = object
extractObject Nothing = head listObstacles 

myGameWire :: (Real t, HasTime t s) => V2 Double -> String -> Wire s e IO (GameState Integer) GameObject
myGameWire pos objid =  proc state -> do
  let object = extractObject $ Map.lookup objid $ gameObjects state 
  newposition <- positionWire myPositionResetFunction pos (velocityWire identityCorrectionFunction $ V2 (negate 10) 0) (constantAcceleration $ V2 0 0) -< state
  let newobject = GameObject {oid=oid object, position=newposition, boundingBox=boundingBox object, gameGraphic=gameGraphic object}
  returnA -< newobject
    
generateWire ::  (Real t, HasTime t s) => [GameObject] -> Wire s e IO (GameState Integer) (Map String GameObject)
generateWire = foldl (\wire object-> combineWire (myGameWire (position object) $ oid object) wire) emptyWire 
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
