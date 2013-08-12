{-# LANGUAGE
    FlexibleContexts
  , UndecidableInstances
  , ConstraintKinds
  , ScopedTypeVariables
  , TypeFamilies
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Gtk.Dict
-- Copyright   :  (c) 2013 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Gtk-specific dict runners - very helpful for small demos / ghci sessions.
--
-- See "Graphics.UI.Toy.Dict" for more information.
--
--------------------------------------------------------------------------------
module Graphics.UI.Toy.Gtk.Dict where

import Data.Traversable       ( Traversable )
import Diagrams.Backend.Cairo ( Cairo )
import Diagrams.Prelude
import Diagrams.Coordinates   ( (&) )

import Graphics.UI.Toy
import Graphics.UI.Toy.Dict
import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Gtk.Diagrams

-- type CairoDiagrammableDict a = DiagrammableDict Cairo R2 a

type GtkToyDict a = ToyDict Gtk Cairo R2 Any a

instance (CairoDiagrammable Any (WithDict d a)) => GtkDisplay (WithDict d a) where
  display = defaultDisplay

-- | @'runPureToy'@ takes the same parameters as 'mkPureToyDict', but also wraps
--   it using 'WithDict', and runs it.
runPureToy
  :: (a -> QDiagram Cairo R2 Any)
  -> (              InputState Gtk -> a -> (a, Bool))
  -> (MouseEvent -> MousePos   Gtk -> a -> a)
  -> (KeyEvent                     -> a -> a)
  -> a -> IO ()
runPureToy df tf mf kf x = runToy $ x `WithDict` mkPureToyDict df tf mf kf

-- | 'runSimpleToy' is a convenient way to run an interactive diagram from just
--   a function from mouse position to @'CairoDiagram'@. Quite useful for ghci.
--
--   @ runSimpleToy f = runMouseToy f (\_ p _ -> p) (0, 0) @
runSimpleToy :: ((Double, Double) -> CairoDiagram) -> IO ()
runSimpleToy f = runMouseToy f (\_ (x, y) _ -> x & y) $ 0 & 0

-- | @'runMouseToy'@ takes the same parameters as 'mkMouseToyDict', but also
--   wraps it using 'WithDict', and runs it.  Useful for ghci.
runMouseToy
  :: forall a. (a -> CairoDiagram)
  -> (Bool -> MousePos Gtk -> a -> a)
  -> a -> IO ()
runMouseToy df mf x = runToy $ x `WithDict`
  (mkMouseToyDict df mf :: GtkToyDict a)

-- | @'runDiagrammableToy' x@ runs any state that is 'Diagrammable' and
--   @'Interactive'@.  The first argument is the initial state, which
--   gets run with ('WithDict') the results of 'mkDiagrammableToyDict'.
runDiagrammableToy :: forall a. CairoInteractive a => a -> IO ()
runDiagrammableToy x = runToy $ x `WithDict`
  (mkDiagrammableToyDict :: GtkToyDict a)

-- | @'runTraversableToy'@ runs any @'Traversable'@ of interactive, diagrammable
--   elements.  The first argument is the initial state, which gets run with
--   ('WithDict') the results of 'mkTraversableToyDict'.
runTraversableToy
  :: forall t a. (Traversable t, CairoInteractive a) => t a -> IO ()
runTraversableToy x = runToy $ x `WithDict`
  (mkTraversableToyDict :: GtkToyDict (t a))
