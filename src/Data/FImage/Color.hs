module Data.FImage.Color
(
  Color(..),

  black,
  white,

  red,
  green,
  blue,

  yellow,
  pink
)
where

data Color = Color { redBeam   :: Float
                   , greenBeam :: Float
                   , blueBeam  :: Float
                   , alpha     :: Float
                   } deriving (Eq, Ord, Show)

black :: Color
black = Color 0 0 0 1

white :: Color
white = Color 1 1 1 1

red :: Color
red = Color 1 0 0 1

green :: Color
green = Color 0 1 0 1

blue :: Color
blue = Color 0 0 1 1

yellow :: Color
yellow = Color 1 1 0 1

pink :: Color
pink = Color 1 0.6 1 1


