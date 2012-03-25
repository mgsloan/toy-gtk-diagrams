{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeFamilies
           , TypeOperators
           , TypeSynonymInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.Slider
-- Copyright   :  (c) 2012 Michael Sloan (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
--
-----------------------------------------------------------------------------
module Graphics.UI.Gtk.Toy.Slider 
  ( Slider
  , sliderHandle, sliderLine, sliderMetric
  , mkToggle, mkSlider
  ) where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Draggable
import Graphics.UI.Gtk.Toy.Utils

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Newtype (Newtype(..))
import Data.Basis (HasBasis(..))
import Data.Label
import Data.Maybe (fromJust)
import Data.MemoTrie (HasTrie)
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Graphics.Rendering.Diagrams.Points

-- TODO: once decent math stuff is in place, make sliders on arbitrary paths.

data Slider b v a = Slider
  { _sliderMetric :: Bijection (->) (Scalar v) a
  , sliderHandle_ :: Draggable (Diagram b v)
  , _sliderLine   :: v
  }

$(mkLabels [''Slider])

sliderHandle :: ( InnerSpace v
                , s ~ Scalar v, Floating s, Ord s)
             => Lens (->) (Slider b v a) (Draggable (Diagram b v))
sliderHandle = lens sliderHandle_
  $ \x s -> let offset' = roundtripBij $ get sliderMetric s . paramBij s
             in s { sliderHandle_ = modify dragOffset offset' x }
 

newtypeBij :: Newtype b a => Bijection (->) a b
newtypeBij = Bij pack unpack

roundtripBij :: Bijection (->) a b -> a -> a
roundtripBij f = bw f . fw f

-- Bijection between 
paramBij :: ( InnerSpace v
            , s ~ Scalar v, Floating s, Ord s )
         => Slider b v a -> Bijection (->) v (Scalar v)
paramBij s
  = Bij (clamp (0, 1) . (/ magnitude l) . (normalized l <.>))
        (lerp zeroV l)
 where l = get sliderLine s
       clamp (f, t) x
         | x < f = f
         | x > t = t
         | otherwise = x

type instance V (Slider b v a) = v

instance Interactive (Slider b R2 a) where
  mouse m i = modifyM sliderHandle (mouse m i)

-- TODO: make vectorspace independent (requires polymorphic stroke)
instance Diagrammable (Slider Cairo R2 a) Cairo R2 where
  toDiagram s = stroke (fromOffsets [get sliderLine s])
             <> toDiagram (get sliderHandle s)

instance ( InnerSpace v, HasBasis v, HasTrie (Basis v)
         , AdditiveGroup (Scalar v), Floating (Scalar v), Ord (Scalar v) )
      => Enveloped (Slider b v a) where
  getEnvelope s = getEnvelope [origin, P $ get sliderLine s]
               <> getEnvelope (get sliderHandle s)

mkSlider :: (Double, Double) -> Diagram b R2 -> R2 -> Slider b R2 Double
mkSlider ivl d = Slider (ivlBij ivl) (mkDraggable (r2 (0, 0)) d)
 where
  -- Creates a bijection between (0, 1) and some other interval
  ivlBij (f, t) = Bij (\x -> x * delta + f)
                      (\x -> x / delta - f)
   where delta = t - f

mkToggle :: Renderable (Path R2) b => Slider b R2 Bool
mkToggle = Slider boolBij (mkDraggable (r2 (0, 0)) $ circle 5) $ r2 (0, 10)
 where
  -- Not a real bijection...
  boolBij = Bij (> 0.5) (fromIntegral . fromEnum)