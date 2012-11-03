{-# LANGUAGE
    ConstraintKinds
  , ExistentialQuantification
  , FlexibleInstances
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TupleSections
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
  , UndecidableInstances
  , KindSignatures
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Diagrams
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Diagrams
  ( 
    CairoDiagram

  -- * Diagrammable class
  , Diagrammable(..)
  , displayDiagram
  , CairoDiagrammable
  , Clickable(..)
  
  -- * Traversable Default Wrappers
  , TToy(..), TDia(..)

  ) where

import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Utils

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Gtk

import Control.Newtype (Newtype, unpack)
import Control.Newtype.TH
import qualified Data.Traversable as T
import qualified Graphics.UI.Gtk as G

type CairoDiagram = Diagram Cairo R2

type CairoDiagrammable = Diagrammable Cairo

-- | Typeclass for things that have a default way of being displayed as a Diagram.
class Diagrammable b a where
  diagram :: a -> Diagram b (V a)

instance Diagrammable b (Diagram b v) where
  diagram = id

-- | Convenience function for implementing the display function of GtkDisplay.
displayDiagram :: (a -> CairoDiagram)
               -> G.DrawWindow -> InputState Gtk -> a -> IO a
displayDiagram f dw _ x = (renderToGtk dw $ f x) >> return x


-- | Clickable things have some concept of which positions are significant when
--   clicked.  Used for buttons, sliders, etc.
class Clickable a where
  clickInside :: a -> Point (V a) -> Bool

instance HasLinearMap v => Clickable (Diagram b v) where
  clickInside d = getAny . runQuery (query d)

-- | Wrapper to make @GtkDisplay@ instances for Diagrammables.
newtype TDia a = TDia a
  deriving (Clickable, Juxtaposable)

$(mkNewtype ''TDia)

type instance V (TToy t a) = V a
type instance V (TDia a)   = V a

deriving instance ( InnerSpace (V a), HasLinearMap (V a), OrderedField (Scalar (V a))
                  , Enveloped a)
                 => Enveloped     (TDia a)

deriving instance ( Transformable a, HasLinearMap (V a))
                 => Transformable (TDia a)

deriving instance ( HasOrigin a, VectorSpace (V a) )
                 => HasOrigin     (TDia a)

instance Diagrammable b a => Diagrammable b (TDia a) where
  diagram = diagram . unpack

-- | Wrapper for making traversable things interactive.
newtype TToy t a = TToy (t a)

$(mkNewtype ''TToy)

instance ( T.Traversable t, Diagrammable b a
         , v ~ V a, HasLinearMap v, InnerSpace v, OrderedField (Scalar v) )
      => Diagrammable b (TToy t a) where
  diagram = T.foldMapDefault diagram . unpack

instance ( T.Traversable t, Interactive ib a )
      => Interactive ib (TToy t a) where
  -- TODO: or together the boolean results
  tick       i = liftA (, True)
               . (TToy `overM` T.traverse (liftA fst . tick i))
  mouse    m i =  TToy `overM` T.traverse (mouse m i)
  keyboard k i =  TToy `overM` T.traverse (keyboard k i)

instance ( T.Traversable t, CairoDiagrammable a, R2 ~ V a)
      => GtkDisplay (TToy t a) where
  display dw i x = displayDiagram diagram dw i x


-- Diagrammable Toy instances

instance Interactive ib (TDia a) where {}

instance ( Diagrammable Cairo a, V a ~ R2 )
      => GtkDisplay (TDia a) where
  display dw i = TDia `overM` displayDiagram diagram dw i

{-
data InteractiveDict b a = InteractiveDict
  { tickFunc     ::               InputState b -> a -> IO (a, Bool)
  , mouseFunc    :: MouseEvent -> InputState b -> a -> IO a
  , keyboardFunc :: KeyEvent   -> InputState b -> a -> IO a
  }

data GtkDisplayDict a = GtkDisplayDict
  { displayFunc :: InputState Gtk -> a -> IO a }

data GtkInteractiveDict a = GtkInteractiveDict
  { interactiveDict :: InteractiveDict Gtk a
  , gtkDisplayDict :: GtkDisplayDict a
  }

data DiagrammableDict b a = DiagrammableDict
  { diagramFunc :: a -> Diagram b (V a) }

data WithDict d a = WithDict a (d a)

instance Interactive b   (WithDict (InteractiveDict b) a) where
  tick       i (WithDict x d) = tickFunc     d   i x
  mouse    m i (WithDict x d) = mouseFunc    d m i x
  keyboard k i (WithDict x d) = keyboardFunc d k i x

instance GtkDisplay      (WithDict GtkDisplayDict      a) where
  display  w i (WithDict x d) = displayFunc  d w i x

instance Interactive Gtk (WithDict GtkInteractiveDict  a) where
  tick       i (WithDict x d) = tickFunc     (interactiveDict d)   i x 
  mouse    m i (WithDict x d) = mouseFunc    (interactiveDict d) m i x
  keyboard k i (WithDict x d) = keyboardFunc (interactiveDict d) k i x

instance GtkDisplay      (WithDict GtkInteractiveDict  a) where
  display  w i (WithDict x d) = displayFunc  (gtkDisplayDict d) w i x

mkDisplayDict :: DiagrammableDict Cairo a -> GtkDisplayDict a
mkDisplayDict = GtkDisplayDict . displayDiagram . diagramFunc

mkGtkToy :: (a -> Diagram Gtk R2)
         -> (MouseEvent -> R2 -> a)
         -> (WithDict GtkInteractiveDict a)


mkPureToy :: 
-}

-- | Wrapper for making a single-item interactive instance
{-
data TProxy a b = TProxy (a :-> b) a

instance T.Traversable (TProxy a) where
  traverse f (TProxy l x) = TProxy . modify l f <$> pure x
-}


-- TODO: build a data-family generating TH library for dictionaries.
--       I data families can't be indexed by constraint??
--       Consider using these dictionaries to implement the utilities above?

{- These could be convenient for trying interactive stuff in ghci

-}
