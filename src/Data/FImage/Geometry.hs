module Data.FImage.Geometry
(

  dist
, distO

, fromPolar
, toPolar
)
where

  import qualified Data.FImage.Geometry.Point      as Point
  import qualified Data.FImage.Geometry.PolarPoint as PolarPoint

  -- | Compute the distance betwen two points.
  dist :: Point.Point -> Point.Point -> Float
  dist (Point.Point x1 y1) (Point.Point x2 y2) = sqrt $ (x2 - x1)**2 + (y2 - y1)**2

  -- | Compute the distance from a given point to the origin.
  distO :: Point.Point -> Float
  distO (Point.Point x y) = sqrt $ x**2 + y**2

  -- | Convert a polar point to a cartesian point.
  fromPolar :: PolarPoint.PolarPoint -> Point.Point
  fromPolar (PolarPoint.PolarPoint rho theta) = Point.mk (rho * cos theta) (rho * sin theta) 

  -- | Convert a cartesian point to a polar point.
  toPolar :: Point.Point -> PolarPoint.PolarPoint
  toPolar p@(Point.Point x y) = PolarPoint.mk (distO p) (atan2 y x)
