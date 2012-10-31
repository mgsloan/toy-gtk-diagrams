{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TemplateHaskell
           , TypeFamilies
           , TypeOperators
           , TypeSynonymInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Slider
-- Copyright   :  (c) 2012 Michael Sloan (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
-- Primitive Slider UI element.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Slider 
  ( Slider, CairoSlider
  , sliderHandle, sliderLine, sliderMetric, sliderValue
  , mkToggle, mkSlider
  ) where

import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Draggable
import Graphics.UI.Toy.Utils

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Newtype (Newtype(..))
import Data.AffineSpace.Point (Point(..))
import Data.Basis (HasBasis(..))
import Data.Label
import Data.Maybe (fromJust)
import Data.MemoTrie (HasTrie)
import Diagrams.Backend.Cairo
import Diagrams.Prelude

-- TODO: once decent math stuff is in place, make sliders on arbitrary paths.

data Slider b v a = Slider
  { _sliderMetric :: Bijection (->) (Scalar v) a
  , _sliderHandle' :: Draggable b (Diagram b v)
  , _sliderLine   :: v
  }

type CairoSlider a = Slider Cairo R2 a

$(mkLabels [''Slider])

--TODO: broken?
sliderHandle :: (OrderedField (Scalar v), AdditiveGroup v, InnerSpace v)
             => Lens (->) (Slider b v a) (Draggable b (Diagram b v))
sliderHandle = lens (get sliderHandle') setter
 where
  setter x s
    = let s' = set sliderHandle' x s 
       in modify sliderValue id s'

sliderValue :: forall b v a. (InnerSpace v, OrderedField (Scalar v))
            => Slider b v a :-> a
sliderValue = lens (\  s -> get (metric s) s)
                   (\x s -> set (metric s) x s)
 where
  metric :: Slider b v a -> Slider b v a :-> a
  metric s = get sliderMetric s `iso` paramLens

sliderPos :: (AdditiveGroup v, OrderedField (Scalar v))
          => Slider b v a :-> v
sliderPos = dragOffset . sliderHandle'

roundtripUnder :: (a :-> b) -> a -> b -> b
roundtripUnder l s v = get l $ set l v s

-- Projects points onto the slider line, yielding a value in (0, 1).
-- The other direction maps the parameter to locations.
paramLens :: (InnerSpace v, OrderedField (Scalar v))
          => Slider b v a :-> Scalar v
paramLens = lens getter setter
 where
  getter s = clamp (0, 1) $ (normalized l <.> get sliderPos s) / magnitude l
   where l = get sliderLine s
  setter x s = set sliderPos (lerp zeroV (get sliderLine s) x) s
  clamp (f, t) x
    | x < f = f
    | x > t = t
    | otherwise = x

type instance V (Slider b v a) = v

--TODO: Make more generic once we have "D2"
instance (Newtype R2 (MousePos ib)) => Interactive ib (Slider b R2 a) where
  mouse m i = modifyM sliderHandle (mouse m i)

-- TODO: make vectorspace independent (requires polymorphic stroke)
instance Diagrammable Cairo (CairoSlider a) where
  diagram s = stroke (fromOffsets [get sliderLine s]) # lc black # lw 2
           <> diagram (get sliderHandle' s)

instance ( InnerSpace v, HasLinearMap v, OrderedField (Scalar v) )
      => Enveloped (Slider b v a) where
  getEnvelope s = getEnvelope [origin, P $ get sliderLine s]
               <> getEnvelope (get sliderHandle' s)

mkSlider :: (AdditiveGroup v, Fractional (Scalar v))
         => (Scalar v, Scalar v) -> Diagram b v -> v -> Slider b v (Scalar v)
mkSlider ivl d = Slider (ivlBij ivl) (mkDraggable zeroV d)
 where
  -- Creates a bijection between (0, 1) and some other interval
  ivlBij (f, t) = Bij (\x -> x * delta + f)
                      (\x -> x / delta - f)
   where delta = t - f

mkToggle :: Renderable (Path R2) b => Slider b R2 Bool
mkToggle = Slider boolBij (mkHandle 5) $ r2 (0, 10)
 where
  -- Not a real bijection...
  boolBij = Bij (> 0.5) (fromIntegral . fromEnum)