{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Examples.Button where

import Data.Default
import Graphics.UI.Toy.Prelude

type State = Transformed (Button Cairo R2)

main = runToy (def :: State)

instance Default State where
  def = translate (100 & 100) $ vcat' (with { sep = 10 }) [but, but, but, but]
   where
    but = mkTransformed (mkDefaultButton "button")
