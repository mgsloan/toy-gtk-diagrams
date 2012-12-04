{-# LANGUAGE FlexibleInstances, TypeFamilies, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Examples.Slider where

import Data.Default
import Data.Dynamic
import Control.Newtype (pack)

import Graphics.UI.Toy.Prelude
import Graphics.UI.Toy.Slider
import Graphics.UI.Toy.Transformed

newtype State = State (Transformed (CairoSlider Double))
  deriving (Interactive Gtk, Diagrammable Cairo R2)

type instance V State = R2

main :: IO ()
main = runToy (def :: State)

instance Default State where
  def = State . translate (100 & 100) $ sli ||| sli ||| sli ||| sli ||| sli ||| sli
   where
    sli = mkTransformed mkDefaultSlider

instance GtkDisplay State where
  display = defaultDisplay