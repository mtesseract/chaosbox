module ChaosBox.Path where

import ChaosBox.Generate
import Data.Foldable (for_)
import Data.RVar
import Graphics.Rendering.Cairo hiding (Path)
import Linear.Metric
import Linear.V2

data Segment' a
  = Segment'
      { segCtrl1 :: a,
        segCtrl2 :: a,
        segEnd :: a
      }
  deriving (Functor, Show)

data Path' a
  = Path'
      { pathBegin :: a,
        pathSegments :: [Segment' a]
      }
  deriving (Functor, Show)

instance Foldable Segment' where
  foldr f accu Segment' {..} =
    foldr f accu [segCtrl1, segCtrl2, segEnd]

instance Traversable Segment' where
  traverse action Segment' {..} =
    Segment'
      <$> action segCtrl1
      <*> action segCtrl2
      <*> action segEnd

type Path = Path' (V2 Double)

instance Foldable Path' where
  foldr f accu (Path' {..}) =
    let accu' = f pathBegin accu
     in foldr (\seg b -> foldr f b seg) accu' pathSegments

instance Traversable Path' where
  traverse action Path' {..} =
    Path' <$> action pathBegin <*> (traverse (traverse action) pathSegments)

pathHomotopy :: Functor f => Double -> f (V2 Double) -> f (V2 Double)
pathHomotopy t path =
  (\a -> a / (pure (t * (norm a) + (1 - t)))) <$> path

distortPath :: RVar Double -> Path -> Generate Path
distortPath distortion path =
  traverse distortPoint path
  where
    distortPoint fa =
      traverse (\a -> (+ a) <$> sampleRVar distortion) fa

drawPath :: Path -> Generate ()
drawPath Path' {..} = do
  cairo $ do
    currentPoint@(V2 currentX currentY) <- (uncurry V2) <$> getCurrentPoint
    let (V2 beginX beginY) = currentPoint + pathBegin
    newPath
    moveTo beginX beginY
    for_ pathSegments $ \Segment' {..} -> do
      let (V2 ctrl1X ctrl1Y) = currentPoint + segCtrl1
          (V2 ctrl2X ctrl2Y) = currentPoint + segCtrl2
          (V2 endX endY) = currentPoint + segEnd
      curveTo ctrl1X ctrl1Y ctrl2X ctrl2Y endX endY
    stroke
    moveTo currentX currentY
