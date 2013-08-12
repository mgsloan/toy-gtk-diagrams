{-# LANGUAGE FlexibleContexts, UndecidableInstances, ConstraintKinds #-}
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

import Graphics.UI.Toy
import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Dict
import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Gtk.Diagrams

type CairoDiagrammableDict a = DiagrammableDict Cairo R2 a

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

{- FIXME: Temporarily disabled.

-- | 'runSimpleToy' is a convenient way to run an interactive diagram from just
--   a function from mouse position to @'CairoDiagram'@. Quite useful for ghci.
--
--   @ runSimpleToy f = runMouseToy f (\_ p _ -> p) (0, 0) @
runSimpleToy :: ((Double, Double) -> QDiagram b R2 q) -> IO ()
runSimpleToy f = runMouseToy f (\_ (x, y) _ -> x & y) $ 0 & 0

-- | @'runMouseToy'@ takes the same parameters as 'mkMouseToyDict', but also wraps
--   it using 'WithDict', and runs it.  Useful for ghci.
runMouseToy
  :: (a -> QDiagram b R2 q)
  -> (Bool -> MousePos ib -> a -> a)
  -> a -> IO ()
runMouseToy df mf x = runToy $ x `WithDict` mkMouseToyDict df mf

-- | @'runDiagrammableToy' x@ runs any state that is 'Diagrammable' and
--   @'Interactive'@.  The first argument is the initial state, which
--   gets run with ('WithDict') the results of 'mkDiagrammableToyDict'.
runDiagrammableToy :: (Interactive Gtk a, Diagrammable Cairo R2 q a)
                   => a -> IO ()
runDiagrammableToy x = runToy $ x `WithDict` mkDiagrammableToyDict


-- | @'runTraversableToy'@ runs any @'Traversable'@ of interactive, diagrammable
--   elements.  The first argument is the initial state, which gets run with
--   ('WithDict') the results of 'mkTraversableToyDict'.
runTraversableToy :: (Traversable t, Interactive Gtk a, Diagrammable Cairo R2 q a
                     , HasLinearMap (V a), OrderedField (Scalar (V a)))
                  => t a -> IO ()
runTraversableToy x = runToy $ x `WithDict` mkTraversableToyDict

-}
