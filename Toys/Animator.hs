{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Animator where

import Graphics.UI.Toy.Prelude
import Data.Colour.SRGB (sRGB)
import Data.Default

data Animator = Animator
  { animTime    :: Double
  , animDiagram :: Double -> CairoDiagram
  }

type instance V Animator = R2

instance Default Animator where
  def = Animator 0 (const mempty)

instance Interactive Gtk Animator where
  tick = simpleTick (\(Animator t f) -> Animator (t + 0.03) f)

instance GtkDisplay Animator where
  display = displayDiagram diagram

instance Diagrammable Cairo Animator where
  diagram (Animator t f) = f t