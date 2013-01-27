{-# LANGUAGE
    FlexibleInstances
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
-- Simple slider UI element.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Slider 
  ( CairoSlider, Slider(..)
 
  -- * Lenses
  , sliderLine, sliderMetric, sliderHandle, sliderValue

  -- * Construction
  , mkSlider, mkDefaultSlider
  , mkToggle, mkDefaultToggle
  ) where

import Prelude hiding ((.))
import Control.Category       ( (.) )
import Control.Newtype        ( Newtype(..) )
import Data.AffineSpace.Point ( Point(..) )
import Data.Label             ( Lens, Bijection(..), (:->), mkLabels, lens, iso, get, set, modify )
import Diagrams.Backend.Cairo ( Cairo )

import Diagrams.Prelude
  ( V, Scalar, R2, VectorSpace(..), InnerSpace(..), HasLinearMap, OrderedField, AdditiveGroup(..)
  , Diagram, Enveloped(..), Renderable, Path
  , (&), origin, magnitude, lerp, normalized
  , (<>), (#), circle, fromOffsets, stroke
  )

import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Draggable
import Graphics.UI.Toy.Gtk

-- TODO: once decent math stuff is in place, make sliders on arbitrary paths.

-- TODO: something simpler than 'sliderMetric'

type CairoSlider a = Slider Cairo R2 a

-- | A @'Slider' b v a@ represents a simple slider UI element.  These can be
--   used as input widgets for any type of data that has a 'Bijection' with
--   the scalars of the vectorspace.  This scalar is gotten by projecting the
--   '_sliderHandle' position onto the '_sliderLine', and mapping into [0, 1].
--
--   This 'Bijection' doesn't need to be a faithful one, but should always map
--   into [0,1] and conversion to scalar and back should be identity.
data Slider b v a = Slider
  { _sliderMetric  :: Bijection (->) (Scalar v) a
  , _sliderHandle' :: Draggable (Diagram b v)
  , _sliderLine    :: v
  }

$(mkLabels [''Slider])

-- | @'mkSlider' f t d v@ creates a linear slider with the 'Diagram' @d@ as a
--   handle.  Applying @'get' 'sliderValue'@ to this slider will result in a
--   value that varies linearly between @f@ and @t@.
mkSlider :: (AdditiveGroup v, Fractional (Scalar v))
         => Scalar v -> Scalar v -> Diagram b v -> v -> Slider b v (Scalar v)
mkSlider f t d = Slider ivlBij (mkDraggable zeroV d)
 where
  -- Creates a bijection between (0, 1) and some other interval
  ivlBij = Bij (\x -> x * delta + f)
               (\x -> x / delta - f)
  delta = t - f

-- | 'mkDefaultSlider' creates a 'Double' slider that has a height of 100 and
--   uses a circle with radius 5 for the handle.
mkDefaultSlider :: Renderable (Path R2) b => Slider b R2 Double
mkDefaultSlider = mkSlider 0 1 (circle 5 # blackLined) $ 0 & 100

-- | @'mkToggle' d v@ creates a 'Bool' slider that has a height of 10 and uses
--   a circle with radius 5 for the handle.
mkToggle :: Renderable (Path R2) b => Draggable (Diagram b R2) -> R2 -> Slider b R2 Bool
mkToggle = Slider $ Bij (> 0.5) (fromIntegral . fromEnum)

-- | 'mkDefaultToggle' creates a 'Bool' slider that has a height of 10 and uses
--   a circle with radius 5 for the handle.
mkDefaultToggle :: Renderable (Path R2) b => Slider b R2 Bool
mkDefaultToggle = mkToggle (mkHandle 5) $ 0 & 10

type instance V (Slider b v a) = v

--TODO: Make more generic once we have "D2"

instance (Newtype R2 (MousePos ib)) => Interactive ib (Slider b R2 a) where
  mouse m i = modifyM sliderHandle (mouse m i)
    where
--      modifyM :: Monad m => (b :-> a) -> (a -> m a) -> b -> m b
      modifyM l f x = do
        a <- f $ get l x
        return $ set l a x

-- TODO: make vectorspace independent (requires polymorphic stroke)

instance Diagrammable Cairo R2 (CairoSlider a) where
  diagram s = stroke (fromOffsets [get sliderLine s]) # blackLined
           <> diagram (get sliderHandle' s)

instance ( InnerSpace v, HasLinearMap v, OrderedField (Scalar v) )
      => Enveloped (Slider b v a) where
  getEnvelope s = getEnvelope [origin, P $ get sliderLine s]
               <> getEnvelope (get sliderHandle' s)

-- | Allows mutation of the slider's handle.  Note that this is not a true
--   lens, because it ensures that the handle is on the slider line.  This
--   is done by round-tripping through 'sliderValue'.
sliderHandle :: (OrderedField (Scalar v), AdditiveGroup v, InnerSpace v)
             => Lens (->) (Slider b v a) (Draggable (Diagram b v))
sliderHandle = lens (get sliderHandle') setter
 where
  setter x s
    = let s' = set sliderHandle' x s 
       in modify sliderValue id s'

-- | 'sliderValue' is a lens that can either yield the current value of the
--   slider, or position the handle based on a new value.
sliderValue :: forall b v a. (InnerSpace v, OrderedField (Scalar v))
            => Slider b v a :-> a
sliderValue = lens (\  s -> get (metric s) s)
                   (\x s -> set (metric s) x s)
 where
  metric :: Slider b v a -> Slider b v a :-> a
  metric s = get sliderMetric s `iso` paramLens

-- Projects handle location onto the slider line, yielding a value in (0, 1).
-- The other direction maps the parameter to handle locations.
paramLens :: (InnerSpace v, OrderedField (Scalar v))
          => Slider b v a :-> Scalar v
paramLens = lens getter setter
 where
  getter s = clamp' (0, 1) $ (normalized l <.> get sliderPos s) / magnitude l
    where l = get sliderLine s
  setter x s = set sliderPos (lerp zeroV (get sliderLine s) x) s
  clamp' (f, t) x
    | x < f = f
    | x > t = t
    | otherwise = x

sliderPos :: (AdditiveGroup v, OrderedField (Scalar v))
          => Slider b v a :-> v
sliderPos = dragOffset . sliderHandle'

{- Fancier slider handle, particularly if part can be under the line:
let { p1 = arc (Deg 0) (Deg 360) # scaleX 5 # scaleY 4
    ; p2 = arc (Deg 0) (Deg 360) # scaleX 3 # scaleY 2 # translateY (-0.5)
    }
  in (p1 <> p2) # stroke # fillRule EvenOdd # fc black # lw 0.0 # lc black
-}