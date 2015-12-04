{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hexmap.Coordinates.Axial
  ( Axial(..)
  , _Axial
  , Hex
  , FractionalHex
  , fractional
  , rounded
  ) where

import           Control.Lens
import           Linear

import           Hexmap.Coordinates.Class

newtype Axial a = Axial (V2 a)
  deriving (Eq, Ord, Show, Read, Functor)

makePrisms ''Axial

instance HasCol Axial a where
    col = _Axial . _x

instance HasRow Axial a where
    row = _Axial . _y

type Hex = Axial Int
type FractionalHex = Axial Double

fractional :: Hex -> FractionalHex
fractional = fmap fromIntegral

rounded :: FractionalHex -> Hex
rounded = fmap round
