{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hexmap.Coordinates.Offset
  ( Offset(..)
  , col, row
  , _Offset
  ) where

import           Control.Lens
import           Linear

import           Hexmap.Coordinates.Class

newtype Offset a = Offset (V2 a)
  deriving (Eq, Ord, Show, Read, Functor)

makePrisms ''Offset

instance HasCol Offset a where
    col = _Offset . _x

instance HasRow Offset a where
    row = _Offset . _y
