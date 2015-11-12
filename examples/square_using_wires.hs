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

initialGameState :: GameState String
initialGameState = createInitialState (Map.fromList [(oid squareObject,squareObject)]) ""

objectName :: String
objectName = assetDirPath ++ "square.png"

squareObject :: GameObject
squareObject = GameObject "square1" (V2 2 2) Hatter.Types.None (Image $ Hatter.Types.Sprite objectName 100 100)

myGameWire :: (Real t, HasTime t s) => Wire s e IO (GameState String) (Map String GameObject)
myGameWire =  proc state -> do
  let object = head $ Map.elems $ gameObjects state
  newposition <- positionWire identityCorrectionFunction (V2 0 0) (velocityWire identityCorrectionFunction $ V2 0 0) (constantAcceleration $ V2 0.1 0.03) -< state
  let newobject = GameObject {oid=oid object, position=newposition, boundingBox=boundingBox object, gameGraphic=gameGraphic object}
  returnA -< (Map.fromList [(oid newobject, newobject)])

    --TODO: Provide function to form new game object with updated position

myEventCheckers :: Map String (GameState String -> ())
myEventCheckers = Map.fromList []

assetDirPath :: String
assetDirPath = "/Users/apple/Documents/Academics/sem9/FP/Hatter/examples/assets/"

canvas :: Canvas
canvas = Canvas 1000 1000 "Square"

main = run myGameDefinition canvas initialGameState

myGameDefinition = GameDefinition { gameWire=myGameWire
                                     , eventCheckers=myEventCheckers
                                     , frameRate=20
                                     , externalState=""
                                     , assetDir=assetDirPath
                                     }
