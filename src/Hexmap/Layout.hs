{-# LANGUAGE TemplateHaskell #-}

module Hexmap.Layout
  ( Layout(..)
  , orientation
  , size
  , hexToPixel
  , pixelToFractional
  , pixelToHex
  ) where

import           Control.Lens
import           Data.Maybe               (fromJust)
import           Linear                   hiding (project, rotate)
import           Linear.Affine

import           Hexmap.Coordinates.Axial
import           Hexmap.Coordinates.Pixel
import           Hexmap.Orientation
import           Hexmap.Util

data Layout = Layout
  { _orientation :: Orientation
  , _size        :: V2 Double
  } deriving (Eq, Show)

makeLenses ''Layout

project :: Layout -> M22 Double
project = view (orientation . forward)

rotate :: Layout -> M22 Double
rotate layout = m22 (cos θ) (-sin θ) (sin θ) (cos θ)
  where
    θ = layout ^. orientation . theta

scale :: Layout -> M22 Double
scale layout = layout ^. size . to scaled

applyLayout :: Layout -> V2 Double -> V2 Double
applyLayout layout v2 = toTransformationMatrix layout !* v2

unapplyLayout :: Layout -> V2 Double -> V2 Double
unapplyLayout layout v2 = inv22' (toTransformationMatrix layout) !* v2
  where inv22' = fromJust . inv22

toTransformationMatrix :: Layout -> M22 Double
toTransformationMatrix l = rotate l !*! scale l !*! project l

hexToPixel :: Layout -> Hex -> Pixel
hexToPixel layout
    = P
    . applyLayout layout
    . fmap fromIntegral
    . view _Axial

pixelToFractional :: Layout -> Pixel -> FractionalHex
pixelToFractional layout
    = Axial
    . unapplyLayout layout
    . view _Point

pixelToHex :: Layout -> Pixel -> Hex
pixelToHex layout = fmap round . pixelToFractional layout
