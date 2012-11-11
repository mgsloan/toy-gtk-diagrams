{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
  #-}
module Toys.Handles where

import Data.Default
import Data.Label

import Graphics.UI.Toy.Prelude
import Graphics.UI.Toy.Dict

main :: IO ()
main = runToy initialState

initialState :: WithDict GtkInteractiveDict [CairoHandle]
initialState
  = mkTraversableToy
  [ set dragOffset (r2 (x, y)) $ mkHandle 5
  | x <- [50,60..100], y <- [50, 60..100]
  ]
