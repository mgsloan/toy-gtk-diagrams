{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeFamilies
  #-}
module Examples.Handles where

import Data.Default
import Data.Label

import Graphics.UI.Toy.Prelude
import Graphics.UI.Toy.Dict

main :: IO ()
main = runTraversableToy initialState

-- | A grid of cairo-handles.
initialState :: [CairoHandle]
initialState = [ set dragOffset (x & y) $ mkHandle 5
               | x <- [50,60..100], y <- [50, 60..100]
               ]
