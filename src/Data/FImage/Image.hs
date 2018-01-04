module Data.FImage.Image
(
  -- * type
  Image,
  ColorImage,
  BImage,

  -- function
  toColor,
  gradient
)
where

  import qualified Data.FImage.Geometry.Point   as Point
  import qualified Data.FImage.Color            as Color
  import qualified Data.FImage.Geometry         as Geometry


  type Image a = Point.Point -> a

  type ColorImage = Image Color.Color

  type BImage = Image Bool


  -- | Convert a BImage to a ColorImage
  -- The 2 colors are the foreground then the background
  toColor :: BImage -> Color.Color -> Color.Color -> ColorImage
  toColor img foreground background p 
    | img p = foreground
    | otherwise = background


  -- | create a gradient (kind of) of 2 color
  gradient :: Float -> Color.Color -> Color.Color -> ColorImage
  gradient width c1 c2 p = Color.Color r g b 1
    where 
      r = Color.redBeam c1 * percentC1 + Color.redBeam c2 * percentC2
      g = Color.greenBeam c1 * percentC1 + Color.greenBeam c2 * percentC2
      b = Color.blueBeam c1 * percentC1 + Color.blueBeam c2 * percentC2

      percentC1 = if distance < 1 then 1 else 1 / (distance - width)
      percentC2 = 1.0 - percentC1

      distance = Geometry.distO p


