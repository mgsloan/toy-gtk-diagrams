{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
  #-}
module Toys.Handles where

import Data.Default
import Data.Label

import Graphics.UI.Toy.Prelude

newtype State = State (TToy [] CairoHandle)
  deriving (Interactive Gtk, GtkDisplay, Diagrammable Cairo)

type instance V State = R2

main :: IO ()
main = runToy (def :: State)

instance Default State where
  def = State $ TToy
      [ set dragOffset (r2 (x, y)) $ mkHandle 5
      | x <- [50,60..100], y <- [50, 60..100]
      ]
