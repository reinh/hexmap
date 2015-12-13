{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hexmap.Coordinates.Conversion where

import           Control.Lens.Iso
import           Hexmap.Coordinates.Axial
import           Hexmap.Coordinates.Offset
import           Hexmap.Coordinates.Pixel
import           Linear

class Conversion a b where
  conversion :: Iso' a b

instance Integral a => Conversion (Offset a) (Axial a) where
  conversion = iso toAxial toOffset
    where
      toAxial (Offset (V2 c r)) = Axial (V2 (c - (r `div` 2)) r)
      toOffset (Axial (V2 c r)) = Offset (V2 (c + (r `div` 2)) r)
