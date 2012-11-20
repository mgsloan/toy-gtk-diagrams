{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Toys.Transformed where

import Data.Default
import Data.Dynamic
import Control.Newtype (pack)

import Graphics.UI.Toy.Prelude
import Graphics.UI.Toy.Slider
import Graphics.UI.Toy.Transformed

type State = Transformed (Slider Cairo R2 Double)

main :: IO ()
main = runToy (def :: State)

instance Default State where
  def = sli ||| sli ||| sli ||| sli ||| sli ||| sli
   where
    sli = mkTransformed mkDefaultSlider