{-# LANGUAGE Arrows #-}
module Hatter.Wires where

import Control.Wire
import Hatter.Types
import Linear
import Data.Map as Map
-- Define wires her which would make it easier for a user to make Games
-- ex- Acc wire, velocity wire, key inout wire, mouse input wire, collision wire etc.

type Velocity = V2 Double
type Acceleration = V2 Double
type VelocityWire s e b = Wire s e IO (GameState b, Acceleration) (V2 Double)
type AccelerationWire s e b= Wire s e IO (GameState b) (V2 Double) 
type CorrectionFunction b = GameState b -> V2 Double -> V2 Double

identityCorrectionFunction :: CorrectionFunction b
identityCorrectionFunction a b = b

constantVelocity :: (Real t, HasTime t s) => V2 Double -> Wire s e IO (GameState b, Acceleration) (V2 Double)
constantVelocity = pure

constantAcceleration :: (Real t, HasTime t s) => V2 Double -> Wire s e IO (GameState b) (V2 Double)
constantAcceleration = pure

velocityWire :: (Real t, HasTime t s) => CorrectionFunction b -> Velocity -> VelocityWire s e b
velocityWire correct = loop
  where
    loop v =
        mkPure $ \ds (state,accel) ->
            let dt = realToFrac (dtime ds) 
                v'  = correct state (v + dt*accel)
            in v `seq` (Right v, loop v')


positionWire_ :: (Real t, HasTime t s) => CorrectionFunction b -> V2 Double -> Wire s e IO (GameState b, Velocity) (V2 Double)
positionWire_ correct = loop
  where
    loop pos= 
      mkPure $ \ds (state, vel) ->
        let dt = realToFrac (dtime ds)
            pos' = correct state (pos + vel*dt)
        in pos `seq` (Right pos, loop pos')

positionWire :: (Real t, HasTime t s) => CorrectionFunction b -> V2 Double -> VelocityWire s e b -> AccelerationWire s e b -> Wire s e IO (GameState b) (V2 Double)
positionWire correct initpos velWire accelWire = let posWire = positionWire_ correct initpos
  in proc state -> do
    accel <- accelWire -< state
    vel <- velWire -< (state,accel)
    pos <- posWire -< (state,vel)
    returnA -< pos

-- | Takes a wire which outputs GameObject and another wire which outputs
-- GameObjects and combines them to give a wire which outputs Map
-- GameObjects
combineObjectWire :: Wire s e IO (GameState b) GameObject -> Wire s e IO (GameState b) GameObject -> Wire s e IO (GameState b) (Map String GameObject)
combineObjectWire wire1 wire2 = proc state -> do
  object1 <- wire1 -< state
  object2 <- wire2 -< state
  returnA -< Map.fromList [(oid object1,object1),(oid object2, object2)]

-- | Takes a wire which outputs GameObject and another wire which outputs
-- Map of GameObjects and combines them to give a wire which outputs Map
-- GameObjects
combineWire :: Wire s e IO (GameState b) GameObject -> Wire s e IO (GameState b) (Map String GameObject) -> Wire s e IO (GameState b) (Map String GameObject)
combineWire wire1 wire2 = proc state -> do
  object <- wire1 -< state
  objectMap <- wire2 -< state
  returnA -< Map.insert (oid object) object objectMap


-- | Takes a wire which outputs map of GameObjects and another wire which outputs
-- Map of GameObjects and combines them to give a wire which outputs Map
-- GameObjects
combineMapWire :: Wire s e IO (GameState b) (Map String GameObject) -> Wire s e IO (GameState b) (Map String GameObject) -> Wire s e IO (GameState b) (Map String GameObject)
combineMapWire wire1 wire2 = proc state -> do
  objectMap1 <- wire1 -< state
  objectMap2 <- wire2 -< state
  returnA -< Map.union objectMap1 objectMap2

-- | Empty Wire which returns no game objects.
emptyWire :: Wire s e IO (GameState b) (Map String GameObject)
emptyWire = proc state -> returnA -< Map.fromList []
