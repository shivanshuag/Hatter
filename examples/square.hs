{-# LANGUAGE Arrows #-}
import Hatter
import Hatter.Types
import Control.Wire as Wire hiding (id)
import Control.Arrow
import Control.Applicative
import FRP.Netwire hiding (id)
import Prelude hiding ((.), id, null, filter)
import Data.Map as Map hiding (foldl)
import Linear

initialGameState :: GameState String
initialGameState = createInitialState [squareObject] ""

objectName :: String
objectName = assetDirPath ++ "square.png"

squareObject :: GameObject
squareObject = GameObject "square1" (V2 2 2) Hatter.Types.None (Image $ Hatter.Types.Sprite objectName 100 100)


myGameWire :: (Real t, HasTime t s) => Wire s e IO (GameState String) [GameObject]
myGameWire =  proc state -> do
    let object = head $ gameObjects state
    let newposition = position object + V2 (10.0 * dt state) 0
      
    --newpositionx <- pos -< 2.1 
    --newpositiony <- pos -< 2.1 
    --let newposition = V2 newpositionx newpositiony + position object
    let newobject = GameObject {id=id object, position=newposition, boundingBox=boundingBox object, gameGraphic=gameGraphic object}
    returnA -< [newobject]
    
    --TODO: Provide function to form new game object with updated position

myEventCheckers :: Map String (GameState String -> ())
myEventCheckers = Map.fromList []

assetDirPath :: String
assetDirPath = "/Users/apple/Documents/Academics/sem9/FP/Hatter/examples/assets/"

myGameDefinition :: GameDefinition (Timed NominalDiffTime()) e String
myGameDefinition = GameDefinition { gameWire=myGameWire
                                         , eventCheckers=myEventCheckers
                                         , frameRate=20
                                         , externalState=""
                                         , assetDir=assetDirPath
                                    }
canvas :: Canvas
canvas = Canvas 1000 1000 "Square"

main = run myGameDefinition canvas initialGameState
