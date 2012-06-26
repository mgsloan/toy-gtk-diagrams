{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , RankNTypes
           , TemplateHaskell
           , TupleSections
           , TypeFamilies
           , UndecidableInstances
           , ImpredicativeTypes
           , ScopedTypeVariables
           , StandaloneDeriving
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Transformed
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Graphics.UI.Toy.Transformed
  ( Transformed(..), mkTransformed, transformed
--  , TThing, ToyThing(..)
  ) where

import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Utils

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Control.Newtype (Newtype, pack, unpack, under, over, overF)
import Control.Newtype.TH
import Data.Data (Data, Typeable, Typeable1)
import Data.Foldable (fold, foldMap)
import Data.Label
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding (over, under)
import Graphics.Rendering.Diagrams.Points
import Debug.Trace

newtype Transformed a = Transformed [(Transformation (V a), a)]
  deriving (Monoid, Semigroup)

deriving instance Typeable1 Transformed
deriving instance (Data a, Data (Transformation (V a))) => Data (Transformed a)

{-
newtype TThing v 
  = TThing
  ( forall a. ( Interactive Gtk a, GtkDisplay a, Enveloped a, Juxtaposable a ) => a )
-- deriving (Interactive Gtk, GtkDisplay, Enveloped, Juxtaposable)

type instance (V (TThing v)) = v

instance Interactive Gtk (TThing v) where
  mouse    m i (TThing x) =       TThing <$> mouse    m i x
  tick       i (TThing x) = first TThing <$> tick       i x
  keyboard k i (TThing x) =       TThing <$> keyboard k i x

instance GtkDisplay (TThing v) where
  display dw i (TThing x) =       TThing <$> display dw i x

instance Enveloped (TThing v) where
  getEnvelope  (TThing x) = getEnvelope x

--instance Juxtaposable TThing where
--  juxtapose = 

-- | Existential wrapper for GtkDisplay, layout-able stuff
type ToyThing v = Transformed (TThing v)

-}

$(mkNewType ''Transformed)

mkTransformed :: HasLinearMap (V a) => a -> Transformed a
mkTransformed = Transformed . (:[]) . (mempty, )

transformed :: (Transformable a) => Transformed a -> [a]
transformed = map (uncurry transform) . unpack

type instance V (Transformed a) = V a

type instance V (InputState Gtk) = R2

instance Transformable (InputState Gtk) where
--  transform t is = is { mousePos = trace (show $ transl $ inv t) $ debug $ transform t $ mousePos is }
  transform t is = is { mousePos = unpack . under P (transform $ inv t) . pack
                                 $ mousePos is }

instance HasLinearMap (V a) => HasOrigin     (Transformed a) where
  moveOriginTo p = translate (origin .-. p)
  
instance HasLinearMap (V a) => Transformable (Transformed a) where
  transform a = Transformed `over` map (first (a <>))

instance ( Enveloped a, HasLinearMap (V a) )
      => Enveloped (Transformed a) where
  getEnvelope = foldMap (\(t, x) -> transform t $ getEnvelope x) . unpack

instance HasStyle a => HasStyle (Transformed a) where
  applyStyle s = Transformed `over` map (second $ applyStyle s)

instance ( v ~ V a, HasLinearMap v, InnerSpace v, OrderedField (Scalar v)
         , Diagrammable Cairo a)
        => Diagrammable Cairo (Transformed a) where
  diagram = foldMap (\(t, x) -> transform t $ diagram x) . unpack

instance ( Enveloped a, HasLinearMap (V a) )
      => Juxtaposable (Transformed a) where
  juxtapose = juxtaposeDefault

overInpT f i = Transformed `overM` mapM (\(t, x) -> (t,) <$> f (transform t i) x)

instance ( Interactive Gtk a, V a ~ R2 )
      => Interactive Gtk (Transformed a) where
  -- TODO: or together the boolean results
  tick     i = liftA (, True)
             . overInpT (\i' -> liftA fst . tick i') i
  mouse    m = overInpT (mouse m)
  keyboard k = overInpT (keyboard k)

instance ( Interactive Gtk a, Diagrammable Cairo a, V a ~ R2 )
      => GtkDisplay (Transformed a) where
  display dw i = displayDiagram diagram dw i

instance ( Clickable a, HasLinearMap (V a) )
        => Clickable (Transformed a) where
  clickInside d p = any (\(t, x) -> clickInside x $ transform t p) $ unpack d

{-
instance Interactive Gtk TThing where
  tick       i = liftA (, True)
               $ TThing `overM` (liftA fst . tick i)
  mouse    m i = TThing `overM` mouse m i
  keyboard k i = TThing `overM` keyboard k i

instance GtkDisplay TThing where
  display = display
-}
