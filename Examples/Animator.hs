{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Examples.Animator where

import Graphics.UI.Toy.Prelude

import Data.Colour.SRGB (sRGB)
import Data.Default
import Data.Time ( UTCTime, UTCTime, getCurrentTime, diffUTCTime )

data Animator = Animator
  { animTime    :: Double
  , animStart   :: Maybe UTCTime
  , animDiagram :: Double -> CairoDiagram
  }

type instance V Animator = R2

instance Default Animator where
  def = Animator 0 Nothing (const mempty)

instance Interactive Gtk Animator where
  tick _ (Animator _ Nothing d) = do
    s <- getCurrentTime
    return $ (Animator 0 (Just s) d, True)

  tick _ (Animator _ (Just s) d) = do
    t' <- realToFrac . (`diffUTCTime` s) <$> getCurrentTime
    return (Animator t' (Just s) d, True)

instance GtkDisplay Animator where
  display = displayDiagram diagram

instance Diagrammable Cairo R2 Animator where
  diagram (Animator t _ f) = f t
