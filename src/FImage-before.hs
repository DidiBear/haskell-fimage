import Data.Foldable as F

import qualified Data.FImage.BMP             as BMP
import qualified Data.FImage.Geometry.Vector as Vector
import qualified Data.FImage.Image           as Image
import qualified Data.FImage.Color           as Color
import qualified Data.FImage.BImage.Simple   as BImage.Simple
import qualified Data.FImage.BImage.Gallery  as BImage.Gallery
import qualified Data.FImage.Transform       as Transform
import qualified Data.FImage.View            as View
import qualified Data.FImage.Window          as Window
import qualified Data.FImage.Render          as Render

-- Helper function. Float transformation.
doTransformFloat ::  (Float -> Transform.Transform) -> Float -> Image.BImage
doTransformFloat t f = t f BImage.Simple.uSquare

-- Helper function. Vector transformation.
doTransformVector ::  (Vector.Vector -> Transform.Transform) -> Float -> Float -> Image.BImage
doTransformVector t dx dy = t v BImage.Simple.uSquare
  where
    v = Vector.mk dx dy

-- Translate a unit square according to a vector defined by two floats.
translateUSquare :: Float -> Float -> Image.BImage
translateUSquare = doTransformVector Transform.translate

-- Translate horizontaly a unit square according to a float.
hTranslateUSquare :: Float -> Image.BImage
hTranslateUSquare = doTransformFloat Transform.hTranslate

-- Translate verticaly a unit square according to a float.
vTranslateUSquare :: Float -> Image.BImage
vTranslateUSquare = doTransformFloat Transform.vTranslate

-- Scale a unit square according to a vector defined by two floats.
scaleUSquare :: Float -> Float -> Image.BImage
scaleUSquare = doTransformVector Transform.scale

-- Scale horizontaly a unit square according to a float.
hScaleUSquare :: Float -> Image.BImage
hScaleUSquare = doTransformFloat Transform.hScale

-- Scale verticaly a unit square according to a float.
vScaleUSquare :: Float -> Image.BImage
vScaleUSquare = doTransformFloat Transform.vScale

-- Scale uniformaly a unit square according to a float.
uScaleUSquare :: Float -> Image.BImage
uScaleUSquare = doTransformFloat Transform.uScale

-- Rotate a unit square according to a float.
rotateUSquare :: Float -> Image.BImage
rotateUSquare = doTransformFloat Transform.rotate

-- Rotate, translate and scale a unit Square
rotateTranslateScaleUSquare :: Float -> Float -> Float -> Float -> Float -> Image.BImage
rotateTranslateScaleUSquare t dx dy sx sy = scale . translate . rotate $ i
  where
    scale     = Transform.scale (Vector.mk sx sy)
    translate = Transform.translate (Vector.mk dx dy)
    rotate    = Transform.rotate t
    i         = BImage.Simple.uSquare

write :: Render.Render a => View.View -> Window.Window -> (String, Image.Image a) -> IO ()
write v w (filename, i) = do
  let bmp = BMP.bmp w v i
  BMP.write filename bmp

basicBImages :: [(String, Image.BImage)]
basicBImages = [        ("hHalfPlane.bmp",   BImage.Simple.hHalfPlane 1)
                      , ("hHalfPlane0.bmp",  BImage.Simple.hHalfPlane0)
                      , ("vHalfPlane.bmp",   BImage.Simple.vHalfPlane 2)
                      , ("vHalfPlane0.bmp",  BImage.Simple.vHalfPlane0)
                      , ("hStrip.bmp",       BImage.Simple.hStrip 2)
                      , ("uHStrip.bmp",      BImage.Simple.uHStrip)
                      , ("vStrip.bmp",       BImage.Simple.vStrip 2)
                      , ("uVStrip.bmp",      BImage.Simple.uVStrip)
                      , ("cross.bmp",        BImage.Simple.cross 2)
                      , ("checker.bmp",      BImage.Simple.checker)
                      , ("altRings.bmp",     BImage.Simple.altRings)
                      , ("disk.bmp",         BImage.Simple.disk 2)
                      , ("uDisk.bmp",        BImage.Simple.uDisk)
                      , ("square.bmp",       BImage.Simple.square 2)
                      , ("uSquare.bmp",      BImage.Simple.uSquare)
                      , ("polarChecker.bmp", BImage.Simple.polarChecker 7)

                      -- transformed boolean images
                      , ("translateUSquare.bmp", translateUSquare 2 3)
                      , ("hTranslateUSquare.bmp", hTranslateUSquare 2)
                      , ("vTranslateUSquare.bmp", vTranslateUSquare 3)
                      , ("scaleUSquare.bmp", scaleUSquare 2 3)
                      , ("hScaleUSquare.bmp", hScaleUSquare 2)
                      , ("vScaleUSquare.bmp", vScaleUSquare 3)
                      , ("uScaleUSquare.bmp", uScaleUSquare 3)
                      , ("rotateUSquare.bmp", rotateUSquare (pi / 4))
                      , ("rotateTranslateScaleUSquare.bmp", rotateTranslateScaleUSquare (pi/4) 1 1 2 1)

                      -- transform boolean images
                      , ("diamond1.bmp", BImage.Gallery.diamond 2 3)
                      , ("diamond2.bmp", BImage.Gallery.diamond 3 2)
                      , ("oval1.bmp", BImage.Gallery.oval 2 3)
                      , ("oval2.bmp", BImage.Gallery.oval 3 2)

                      -- algebra boolean images
                      , ("annulus.bmp", BImage.Gallery.annulus 4 2)
                      , ("checkerDisk.bmp", BImage.Gallery.checkerDisk 4)
                      , ("checkerSquare.bmp", BImage.Gallery.checkerSquare 6)
                      , ("checkerAnnulus.bmp", BImage.Gallery.checkerAnnulus 4 1)
                      , ("octogon.bmp", BImage.Gallery.octogon 3)
                      , ("xorCircles2.bmp", BImage.Gallery.xorCircles2 2 1)
                      , ("checkerXORCircles2.bmp", BImage.Gallery.checkerXORCircles2 2 1)
                      , ("xorCircles4.bmp", BImage.Gallery.xorCircles4 2 1)
                      , ("xorCircles8.bmp", BImage.Gallery.xorCircles8 2 1)
                      , ("eclipse.bmp", BImage.Gallery.eclipse 3 2 0.75)
                      , ("lineCircles.bmp", BImage.Gallery.lineCircles 2)
                      , ("timeTunnel.bmp", BImage.Gallery.timeTunnel)]


main :: IO ()
main = do
  let v   = View.mk0 8 8
  let w   = Window.mk 256 256
  F.mapM_ (write v w) basicBImages
