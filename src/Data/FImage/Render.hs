{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.FImage.Render
(
  Render(..)
)
where

  import qualified Data.Word as Word
  import qualified Data.FImage.Color as Color

  -- | Render type class.
  class Render a where
    render :: a -> [Word.Word8]

  -- Render a boolean as four word8.
  instance Render Bool where
    render False = [255, 255, 255, 255]
    render True  = [  0,   0,   0, 255]

  instance Render Color.Color where
    render (Color.Color r g b a) = map (floor . (* 255)) [r, g, b, a]
