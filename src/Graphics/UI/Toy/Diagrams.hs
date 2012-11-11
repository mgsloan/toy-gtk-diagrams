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
  ) where

import Graphics.UI.Toy.Gtk

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Gtk

import Graphics.UI.Gtk (DrawWindow)

type CairoDiagram = Diagram Cairo R2

type CairoDiagrammable = Diagrammable Cairo


-- | Typeclass for things that have a default way of being displayed as a Diagram.
class Diagrammable b a where
  diagram :: a -> Diagram b (V a)

instance Diagrammable b (Diagram b v) where
  diagram = id

-- | Convenience function for implementing the display function of GtkDisplay.
displayDiagram :: (a -> CairoDiagram)
               -> DrawWindow -> InputState Gtk -> a -> IO a
displayDiagram f dw _ x = (renderToGtk dw $ f x) >> return x


-- | Clickable things have some concept of which positions are significant when
--   clicked.  Used for buttons, sliders, etc.
class Clickable a where
  clickInside :: a -> Point (V a) -> Bool

instance HasLinearMap v => Clickable (Diagram b v) where
  clickInside d = getAny . runQuery (query d)
