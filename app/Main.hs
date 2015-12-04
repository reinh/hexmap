{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Control.Applicative
import           Data.Maybe                   (fromMaybe)
import           Diagrams.Backend.SVG
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Size
import           Diagrams.TwoD.Text
import           Lucid
import           Lucid.Html5
import           Lucid.Svg                    hiding (rotate)

import           Hexmap.Coordinates.Axial
import           Hexmap.Coordinates.Class
import           Hexmap.Layout
import           Hexmap.Orientation

hexSize, mapSize :: Int
hexSize = 20
mapSize = 10

orient :: OrientationTop
orient = Pointy

data Tile = Land | Water

type MapData = [(Hex, Tile)]

mapData :: [(Hex, Tile)]
mapData = [(Axial (V2 c r), Land) | c <- [2..mapSize - 2], r <- [2..mapSize - 2]]

lookupTile :: Hex -> MapData -> Tile
lookupTile hex = fromMaybe Water . lookup hex

layout :: Layout
layout = Layout (mkOrientation orient) (fromIntegral hexSize)

rads :: Angle Double
rads = (θ - pi / 6) @@ rad
  where
    θ = layout ^. orientation . theta

showHex :: Show a => Axial a -> String
showHex hex = mconcat [showL col , ",", showL row]
  where
    showL l = show (hex ^. l)


tileDiagram hex = label <> tile
  where
    label = text (showHex hex)
          # fontSizeL (fromIntegral hexSize * (2 / 3))
          # fontWeight FontWeightBold
          # fc white
          # opacity 0.5
    tile = hexagon (fromIntegral hexSize)
         # lw 1
         # lc (darken 1.2 color)
         # fc color
         # rotate rads
    color =
      case lookupTile hex mapData of
        Land  -> lightgreen
        Water -> lightblue

main :: IO ()
main = defaultMain (font "Helvetica" mapDiagram)
  where
    mapDiagram = mconcat (hexDiagram <$> hexes mapSize)
      where
          hexes n = [Axial (V2 x y) | x <- [0..n], y <- [0..n]]
    hexDiagram hex = moveTo (hexToPixel layout hex) (tileDiagram hex)

-- renderSvg = renderDia SVG (SVGOptions spec Nothing "")
--   where
--     spec = mkSizeSpec2D (Just 800) (Just 800)


-- renderHtmlIO = renderToFile "index.html" $ do
--   doctypehtml_ $ do
