{-# LANGUAGE TemplateHaskell #-}

module Hexmap.Orientation where

import           Control.Lens
import           Linear

import           Hexmap.Util

data OrientationTop = Pointy | Flat

data Orientation = Orientation
  { _forward :: M22 Double -- ^ 2x2 forward matrix
  , _theta   :: Double     -- ^ start angle in radians
  } deriving (Eq, Show, Read)

makeLenses ''Orientation

mkOrientation :: OrientationTop -> Orientation
mkOrientation Pointy = Orientation
  { _forward = m22 sqrt_3 sqrt_3_2 0 1.5
  , _theta = 0
  }
mkOrientation Flat = Orientation
  { _forward = m22 sqrt_3 sqrt_3_2 0 1.5
  , _theta = pi / 6
  }
