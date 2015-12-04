{-# LANGUAGE MultiParamTypeClasses #-}

module Hexmap.Coordinates.Class
  ( HasCol
  , col
  , HasRow
  , row
  ) where

import           Control.Lens

class HasCol s a where
  col :: Lens' (s a) a

class HasRow s a where
  row :: Lens' (s a) a
