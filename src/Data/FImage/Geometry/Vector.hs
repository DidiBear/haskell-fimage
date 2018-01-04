module Data.FImage.Geometry.Vector
(
  -- * type definition
  Vector(..)

  -- * constructing
, mk
, mk2

  -- * transforming
, revX
, revY
, revXY
, invX
, invY
, invXY
)
where

  -- | Vector type definition
  data Vector = Vector { dx :: Float, dy :: Float }
                deriving (Show, Eq, Ord)

  -- | Make a vector (dx, dy) from two floats `dx`and `dy`.
  mk :: Float -> Float -> Vector
  mk x y = Vector x y

  -- | Make a vector '(dz, dz)'' from a float 'dz'.
  mk2 :: Float -> Vector
  mk2 dz = mk dz dz

  -- | Make the vector (-dx, dy) from vector (dx, dy).
  revX :: Vector -> Vector
  revX (Vector x y) = mk (negate x) y

  -- | Make the vector (dx, -dy) from vector (dx, dy).
  revY :: Vector -> Vector
  revY (Vector x y) = mk x (negate y)

  -- | Make the vector (-dx, -dy) from vector (dx, dy).
  revXY :: Vector -> Vector
  revXY = revX . revY

  -- | Make the vector (1/dx, dy) from vector (dx, dy).
  invX :: Vector -> Vector
  invX (Vector x y) = mk (1 / x) y

  -- | Make the vector (dx, 1/dy) from vector (dx, dy).
  invY :: Vector -> Vector
  invY (Vector x y) = mk x (1 / y)

  -- | Make the vector (1/dx, 1/dy) from vector (dx, dy).
  invXY :: Vector -> Vector
  invXY = invX . invY
