module ChaosBox.Coordinates (normalizeY, normalizeX, normalize, normalizeY', normalizeX', normalize') where

import ChaosBox.Generate
import Control.Monad.Reader
import Linear.V2

normalizeY :: Fractional a => V2 a -> Generate a
normalizeY (V2 _x y) = do
  GenerateCtx {..} <- ask
  let height = realToFrac gcHeight
  pure $ (y + (height / 2)) / height

normalizeX :: Fractional a => V2 a -> Generate a
normalizeX (V2 x _y) = do
  GenerateCtx {..} <- ask
  let width = realToFrac gcWidth
  pure $ (x + (width / 2)) / width

normalize :: Fractional a => V2 a -> Generate (a, a)
normalize p = do
  y <- normalizeY p
  x <- normalizeX p
  pure (x, y)

normalizeY' :: Fractional a => V2 a -> Generate a
normalizeY' (V2 _x y) = do
  GenerateCtx {..} <- ask
  let height = realToFrac gcHeight
  pure $ y / (height / 2)

normalizeX' :: Fractional a => V2 a -> Generate a
normalizeX' (V2 x _y) = do
  GenerateCtx {..} <- ask
  let width = realToFrac gcWidth
  pure $ x / (width / 2)

normalize' :: Fractional a => V2 a -> Generate (a, a)
normalize' p = do
  y <- normalizeY' p
  x <- normalizeX' p
  pure (x, y)
