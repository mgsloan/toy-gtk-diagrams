{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
  #-}
module Toys.Handles where

import Data.Default

import Graphics.UI.Gtk.Toy.Prelude

newtype State = State (TToy [] (CairoDraggable CairoDiagram))
  deriving (Interactive, GtkInteractive, Diagrammable Cairo)

type instance V State = R2

main :: IO ()
main = runToy (def :: State)

instance Default State where
  def = State $ TToy
      [ mkDraggable (r2 (x, y)) (circle 5 :: CairoDiagram)
      | x <- [50,60..100], y <- [50, 60..100]
      ]