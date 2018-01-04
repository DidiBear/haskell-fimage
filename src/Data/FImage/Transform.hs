module Data.FImage.Transform
(
-- * type
Transform

-- * Transforming boolean images
, translate
, hTranslate
, vTranslate
, scale
, hScale
, vScale
, uScale
, rotate
)
where

  import qualified Data.FImage.Image          as BImage
  import qualified Data.FImage.Geometry.Point  as Point
  import qualified Data.FImage.Geometry.Vector as Vector

  -- | Point transformation type definition.
  type TransformPoint = Point.Point -> Point.Point

  -- | Boolean image transformation type definition.
  type Transform = BImage.BImage -> BImage.BImage

  -- | Add a vector with a point:
  -- plusPoint (dx, dy) (x, y) = (x + dx, y + dy).
  -- plusPoint :: Vector.Vector -> TransformPoint

  -- | Multiply a vector with a point:
  -- multPoint (dx, dy) (x, y) = (x * dx, y * dy).
  -- multPoint :: Vector.Vector -> TransformPoint

  -- | Translate a point according to a given vector:
  -- translatePoint (dx, dy) (x, y) = (x + dx, y + dy)
  translatePoint :: Vector.Vector -> TransformPoint
  translatePoint (Vector.Vector dx dy) (Point.Point x y) = Point.mk (x + dx) (y + dy)

  -- | Scale a point according to a given vector:
  -- scalePoint (dx, dy) (x, y) = (x * dx, y * dy).
  scalePoint :: Vector.Vector -> TransformPoint
  scalePoint (Vector.Vector dx dy) (Point.Point x y) = Point.mk (x * dx) (y * dy) 

  -- | Scale uniformaly a point according to a given float:
  -- uScalePoint f (x, y) = (f * x, f * y).
  uScalePoint :: Float -> TransformPoint
  uScalePoint f (Point.Point x y) = Point.mk (x * f) (y * f) 

  -- | Rotate a point acoording to a given angle t:
  -- rotatePoint t (x, y) = (x cos(t) - y sint(t), x sin(t) + y cos(t)).
  rotatePoint :: Float -> TransformPoint
  rotatePoint t (Point.Point x y)  = Point.mk (x * cos t - y * sin t) (x * cos t + y * sin t) 


  -- | Translate a boolean image according to a vector.  
  -- translate :: Vector.Vector -> (Point -> Bool) -> Point -> Bool
  translate :: Vector.Vector -> Transform
  translate v b p = b (translatePoint (Vector.revXY v) p)

  -- | Translate horizontaly a boolean image according to a float.
  hTranslate :: Float -> Transform
  hTranslate n = translate (Vector.mk n 0)

  -- | Translate verticaly a boolean image according to a float.
  vTranslate :: Float -> Transform
  vTranslate n = translate (Vector.mk 0 n)

  -- | Scale a boolean image according to a vector.
  -- scale :: Vector.Vector -> (Point -> Bool) -> Point -> Bool
  scale :: Vector.Vector -> Transform
  scale v b p = b (scalePoint (Vector.invXY v) p)

  -- | Scale horizontaly a boolean image according to a float.
  hScale :: Float -> Transform
  hScale n = scale (Vector.mk n 0)

  -- | Scale verticaly a boolean image according to a float.
  vScale :: Float -> Transform
  vScale n = scale (Vector.mk 0 n)

  -- | Scale uniformaly a boolean image according to a float.
  -- uScale :: Float -> (Point -> Bool) -> Point -> Bool  
  uScale :: Float -> Transform
  uScale n b p = b (uScalePoint (1/n) p)

  -- | Rotate uniformaly a boolean image according to a float.
  -- rotate :: Float -> (Point -> Bool) -> Point -> Bool  
  rotate :: Float -> Transform
  rotate t b p = b (rotatePoint (-t) p)
