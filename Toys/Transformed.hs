{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Toys.Transformed where

import Data.Default
import Data.Dynamic
import Control.Newtype (pack)

import Graphics.UI.Gtk.Toy.Prelude
import Graphics.UI.Gtk.Toy.Slider
import Graphics.UI.Gtk.Toy.Transformed

type State = Transformed (Slider Cairo R2 Double)

main :: IO ()
main = runToy (def :: State)

instance Default State where
  def = sli ||| sli ||| sli ||| sli ||| sli ||| sli
   where
   	sli = mkTransformed $ mkSlider (0, 1) (circle 5) (r2 (0, 100))