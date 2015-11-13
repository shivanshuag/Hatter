{-# LANGUAGE Arrows #-}
import Hatter
import Hatter.Types
import Hatter.Wires
import Data.Map as Map
import Control.Wire as Wire hiding (id)
import Linear

g :: Double
g = 6.6e-13

assetdir :: String
assetdir = "examples/assets/"
data Mass = Mass{mass1 :: Double
                ,mass2 :: Double}

mass = Mass{mass1=1e+14
           ,mass2=2e+14}

body1 :: GameObject
body1 = GameObject{oid="body1"
                  ,position=V2 10 300
                  ,boundingBox=Circle (V2 60 60) 60
                  ,gameGraphic=Image Sprite{file=assetdir++"planet1.png",width=120,height=120}
                  }

body2 :: GameObject
body2 = GameObject{oid="body2"
                  ,position=V2 700 300
                  ,boundingBox=Circle (V2 50 50) 50
                  ,gameGraphic=Image Sprite{file=assetdir++"planet2.png",width=100,height=100}
                  }

acceleration1Wire :: Wire s e IO (GameState Mass) (V2 Double)
acceleration1Wire = proc state -> do
  let V2 x y = negate 1 * posdiff state
  let dis = rcube $ V2 x y
  let mass = mass2 $ extraState state
  returnA -< V2 (g*mass*x/dis) (g*mass*y/dis)

rcube :: V2 Double -> Double
rcube (V2 x y)= (x*x + y*y)**1.5

acceleration2Wire :: Wire s e IO (GameState Mass) (V2 Double)
acceleration2Wire = proc state -> do
  let V2 x y = posdiff state
  let dis = rcube $ V2 x y
  let mass = mass1 $ extraState state
  returnA -< V2 (g*mass*x/dis) (g*mass*y/dis)

posdiff :: GameState Mass -> V2 Double
posdiff state =
  let objects = gameObjects state
      (object1, object2) = (Map.lookup "body1" objects, Map.lookup "body2" objects)
  in case (object1, object2) of
    (Just o1, Just o2) -> (position o1 + V2 60 60)  - (position o2 + V2 50 50)
    (_,_) -> V2 0 0

body1Wire :: (Monoid e, Real t, HasTime t s) => Wire s e IO (GameState Mass) GameObject
body1Wire = proc state ->
  case Map.lookup "body1" $ gameObjects state of
    Just object -> do
      newposition <- positionWire identityCorrectionFunction (V2 10 300) (velocityWire identityCorrectionFunction $ V2 0 5) acceleration1Wire -< state
      let newobject = GameObject {oid=oid object, position=newposition, boundingBox=boundingBox object, gameGraphic=gameGraphic object}
      returnA -< newobject
    Nothing -> returnA -< body1


body2Wire :: (Monoid e, Real t, HasTime t s) => Wire s e IO (GameState Mass) GameObject
body2Wire = proc state ->
  case Map.lookup "body2" $ gameObjects state of
    Just object -> do
      newposition <- positionWire identityCorrectionFunction (V2 10 300) (velocityWire identityCorrectionFunction $ V2 0 $ negate 5) acceleration2Wire -< state
      let newobject = GameObject {oid=oid object, position=newposition, boundingBox=boundingBox object, gameGraphic=gameGraphic object}
      returnA -< newobject
    Nothing -> returnA -< body1

myGameDefinition = GameDefinition {gameWire=myGameWire
                                   ,eventCheckers=Map.fromList []
                                   ,frameRate=20
                                   ,externalState=mass
                                   ,assetDir=assetdir
                                     }

main = run
