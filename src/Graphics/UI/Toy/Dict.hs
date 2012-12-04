{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , TupleSections
  , TypeFamilies
  , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Dict
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Data structure versions of type classes relevant for \"toys\".  While it's
-- a bit of a design smell to have these value-level dictionaries for these
-- typeclasses, they can be very handy for \"toy\" demos and ghci.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Dict
  ( 
-- * Dictionary
    WithDict(..), Dictionary(..), withDict

-- * Interactive Dictionary
  , InteractiveDict(..), mkInteractiveDict

-- * Diagrammable Dictionary
  , DiagrammableDict(..)

-- * GtkInteractive Dictionary
  , GtkInteractiveDict(..)
  , mkPureToyDict, runPureToy
  , mkMouseToyDict, runMouseToy
  , runSimpleToy
  , mkDiagrammableToyDict, runDiagrammableToy
  , mkTraversableToyDict, runTraversableToy
  ) where

import Control.Arrow          ( first )
import Data.Default           ( Default(..) )
import Data.Traversable       ( Traversable(..), foldMapDefault )
import Diagrams.Backend.Cairo ( Cairo )
import Graphics.UI.Gtk        ( DrawWindow )

import Diagrams.Prelude
  ( V, R2, Diagram, InnerSpace, HasLinearMap, AdditiveGroup, VectorSpace, Scalar
  , Juxtaposable(..), HasOrigin(..), Enveloped(..), Transformable(..)
  , juxtaposeDefault
  , liftA, mempty )

import Graphics.UI.Toy.Gtk
  ( Gtk, GtkDisplay(..), Interactive(..), InputState, MousePos, MouseEvent, KeyEvent
  , runToy, mousePos, keyHeld, simpleMouse, simpleKeyboard )

import Graphics.UI.Toy.Diagrams
  ( Diagrammable(..), Clickable(..), defaultDisplay
  , CairoDiagram, CairoDiagrammable)


-------------------------------------------------------------------------------
-- WithDict data / Dictionary
-------------------------------------------------------------------------------

-- | @'WithDict' d a@ pairs a dictionary-like datatype along with a value.
--
-- In some ways this is like OOP, where a pointer to the vtable travels around
-- with the data.  For many typeclasses, we're able to write instances for
-- @WithDict@, such that the data is processed with the value-provided method
-- dictionary.
--
-- One thing to note is that, somewhat arbitrarily, the decision has been made
-- that this particular \"WithDict\" forwards along the implementations of
-- 'Juxtaposable', 'HasOrigin', 'Enveloped', 'Transformable', and 'Clickable'.
-- This means that creating \"*Dict\"s for these typeclasses, for use 'WithDict'
-- is not possible.
data WithDict d a = WithDict a (d a)

type instance V (WithDict d a) = V a

instance (Enveloped a, HasOrigin a, HasLinearMap (V a))
      => Juxtaposable (WithDict d a) where
  juxtapose = juxtaposeDefault

instance (HasOrigin a, VectorSpace (V a))
      => HasOrigin (WithDict d a) where
  moveOriginTo p (WithDict x d) = moveOriginTo p x `WithDict` d

instance (Enveloped a, v ~ V a, InnerSpace v, HasLinearMap v)
      => Enveloped (WithDict d a) where
  getEnvelope (WithDict x _) = getEnvelope x

instance (Transformable a, HasLinearMap (V a))
      => Transformable (WithDict d a) where
  transform t (WithDict x d) = transform t x `WithDict` d

instance Clickable a => Clickable (WithDict d a) where
  clickInside (WithDict x _) = clickInside x

instance (CairoDiagrammable (WithDict d a)) => GtkDisplay (WithDict d a) where
  display = defaultDisplay

-- | An instance of 'Dictionary' provides a 'dict', populated from an instance
--   of the actual typeclass.  In other words, 
class Dictionary d where
  dict :: d

-- | @'withDict' x@ wraps a value with a reified version of a dictionary.
withDict :: Dictionary (d a) => a -> WithDict d a
withDict x = WithDict x dict


-------------------------------------------------------------------------------
-- Interactive Dictionary
-------------------------------------------------------------------------------

-- | @'InteractiveDict' b a@ wraps all of the methods needed to make an
--   @'Interactive' b a@ instance.
data InteractiveDict b a = InteractiveDict
  { tickFunc     ::               InputState b -> a -> IO (a, Bool)
  , mouseFunc    :: MouseEvent -> InputState b -> a -> IO a
  , keyboardFunc :: KeyEvent   -> InputState b -> a -> IO a
  }

instance Interactive b a => Dictionary (InteractiveDict b a) where
  dict = InteractiveDict tick mouse keyboard

instance Interactive b (WithDict (InteractiveDict b) a) where
  tick       i (WithDict x d) = fmap (first (`WithDict` d)) $ tickFunc     d   i x
  mouse    m i (WithDict x d) = fmap (`WithDict` d)         $ mouseFunc    d m i x
  keyboard k i (WithDict x d) = fmap (`WithDict` d)         $ keyboardFunc d k i x

instance Default (InteractiveDict b a) where
  def = InteractiveDict
    { tickFunc = const $ return . (, False)
    , mouseFunc = const . const $ return
    , keyboardFunc = const . const $ return
    }

-- | 'mkInteractiveDict' takes simplified versions of 'tick', 'mouse',
--   and 'keyboard', and uses them to implement an @'InteractiveDict' b a@.
mkInteractiveDict
  :: (              InputState b -> a -> (a, Bool))
  -> (MouseEvent -> MousePos b   -> a -> a)
  -> (KeyEvent                   -> a -> a)
  -> InteractiveDict b a
mkInteractiveDict tf mf kf = InteractiveDict
  (\i -> return . tf i)
  (simpleMouse mf)
  (simpleKeyboard kf)


-------------------------------------------------------------------------------
-- Diagrammable Dictionary
-------------------------------------------------------------------------------

-- | @'DiagrammableDict' b v a@ wraps all of the methods needed to make an
--   instance of @'Diagrammable' b v a@.
data DiagrammableDict b v a = DiagrammableDict
  { diagramFunc :: a -> Diagram b v }

type CairoDiagrammableDict a = DiagrammableDict Cairo R2 a

instance Diagrammable b v a => Dictionary (DiagrammableDict b v a) where
  dict = DiagrammableDict diagram

instance ( InnerSpace v, HasLinearMap v
         , AdditiveGroup (Scalar v), Ord (Scalar v), Floating (Scalar v) )
      => Default (DiagrammableDict b v a) where
  def = DiagrammableDict $ const mempty


-------------------------------------------------------------------------------
-- GtkInteractive Dictionary
-------------------------------------------------------------------------------

-- | @'GtkInteractiveDict' a@ ought to be related to the constraint synonym:
--
--   @type 'GtkInteractive' a = (Interactive Gtk a, GtkDisplay a)@
-- 
--   However, since there is no @GtkDisplayDict@, its functionality is instead
--   mostly provided by 'DiagrammableDict'.  This also allows
--   'GtkInteractiveDict' to be more useful, as they can be combined and used
--   with 'Transformed'.
--
--   So, this contains a @'GtkDisplayDict' a@ and @'InteractiveDict' 'Gtk' a@.
--   With these together, we have all of the functions necessary to describe a
--   diagrams toy.  @'WithDict' 'GtkInteractiveDict' a@ is a good type to
--   describe toys that construct their implementation at runtime.
data GtkInteractiveDict a = GtkInteractiveDict
  { diagrammableDict :: CairoDiagrammableDict a
  , interactiveDict :: InteractiveDict Gtk a
  }

instance (Interactive Gtk a, CairoDiagrammable a) => Dictionary (GtkInteractiveDict a) where
  dict = GtkInteractiveDict dict dict

instance ( InnerSpace (V a), HasLinearMap (V a)
         , AdditiveGroup (Scalar (V a)), Ord (Scalar (V a)), Floating (Scalar (V a)) )
      => Default (GtkInteractiveDict a) where
  def = GtkInteractiveDict def def

instance Interactive Gtk (WithDict GtkInteractiveDict a) where
  tick       i (WithDict x d) = fmap (first (`WithDict` d)) $ tickFunc     (interactiveDict d)   i x
  mouse    m i (WithDict x d) = fmap        (`WithDict` d)  $ mouseFunc    (interactiveDict d) m i x
  keyboard k i (WithDict x d) = fmap        (`WithDict` d)  $ keyboardFunc (interactiveDict d) k i x

instance Diagrammable Cairo R2 (WithDict GtkInteractiveDict a) where
  diagram (WithDict x d) = diagramFunc (diagrammableDict d) x

-- | @'mkPureToyDict'@ takes all of the parameters of @'mkInteractiveDict'@, with
--   an additional function to transform the state to a diagram.  The parameters
--   correspond to 'diagram', 'tick', 'mouse', and 'keyboard', respectively.
mkPureToyDict
  :: (a -> CairoDiagram)
  -> (              InputState Gtk   -> a -> (a, Bool))
  -> (MouseEvent -> (Double, Double) -> a -> a)
  -> (KeyEvent                       -> a -> a)
  -> GtkInteractiveDict a
mkPureToyDict df tf mf kf = GtkInteractiveDict
  (DiagrammableDict df)
  (mkInteractiveDict tf mf kf)

-- | @'runPureToy'@ takes the same parameters as 'mkPureToyDict', but also wraps
--   it using 'WithDict', and runs it.
runPureToy
  :: (a -> CairoDiagram)
  -> (              InputState Gtk -> a -> (a, Bool))
  -> (MouseEvent -> MousePos   Gtk -> a -> a)
  -> (KeyEvent                     -> a -> a)
  -> a -> IO ()
runPureToy df tf mf kf x = runToy $ x `WithDict` mkPureToyDict df tf mf kf

-- | @'mkMouseToyDict'@ is a convenience constructor that takes a diagram display
--   function and a function for transforming the state based on whether the
--   mouse is clicked and its position.
mkMouseToyDict
  :: (a -> CairoDiagram)
  -> (Bool -> MousePos Gtk -> a -> a)
  -> GtkInteractiveDict a
mkMouseToyDict df mf = GtkInteractiveDict
  (DiagrammableDict df)
  (InteractiveDict
    (\_ x -> return (x, False))
    (\_ i x -> return $ mf (keyHeld "Mouse1" i || keyHeld "Mouse2" i) (mousePos i) x)
    (\_ _ x -> return x))

-- | @'runMouseToy'@ takes the same parameters as 'mkMouseToyDict', but also wraps
--   it using 'WithDict', and runs it.  Useful for ghci.
runMouseToy
  :: (a -> CairoDiagram)
  -> (Bool -> MousePos Gtk -> a -> a)
  -> a -> IO ()
runMouseToy df mf x = runToy $ x `WithDict` mkMouseToyDict df mf

-- | 'runSimpleToy' is a convenient way to run an interactive diagram from just
--   a function from mouse position to @'CairoDiagram'@. Quite useful for ghci.
--
--   @ runSimpleToy f = runMouseToy f (\_ p _ -> p) (0, 0) @
runSimpleToy :: ((Double, Double) -> CairoDiagram) -> IO ()
runSimpleToy f = runMouseToy f (\_ p _ -> p) (0, 0)

-- | 'mkDiagrammableToyDict' yields a @'GtkInteractiveDict'@ for any type that is
--   'Diagrammable' and 'Interactive'.
mkDiagrammableToyDict :: (Interactive Gtk a, Diagrammable Cairo R2 a)
                      => GtkInteractiveDict a
mkDiagrammableToyDict = GtkInteractiveDict (DiagrammableDict $ diagramFunc dict) dict

-- | @'runDiagrammableToy' x@ runs any state that is 'CairoDiagrammable' and
--   @'Interactive' 'Gtk'@.  The first argument is the initial state, which
--   gets run with ('WithDict') the results of 'mkDiagrammableToyDict'.
runDiagrammableToy :: (Interactive Gtk a, Diagrammable Cairo R2 a)
                   => a -> IO ()
runDiagrammableToy x = runToy $ x `WithDict` mkDiagrammableToyDict

-- | @'mkTraversableToyDict'@ provides a 'GtkInteractiveDict' for any
--   @'Traversable'@ of interactive, diagrammable elements.  The resulting
--   dictionary implements these such that every subcomponent receives all
--   'tick', 'mouse', 'keyboard', and 'diagram' call.  The resulting
--   'CairoDiagram's are merged together via '(<>)'
mkTraversableToyDict :: (Traversable t, Interactive Gtk a, CairoDiagrammable a)
                     => GtkInteractiveDict (t a)
mkTraversableToyDict = GtkInteractiveDict
  (DiagrammableDict . foldMapDefault $ diagramFunc dict)
  (InteractiveDict
  -- TODO: or together the boolean results
    { tickFunc     = \  i x -> liftA (, True) $ traverse (liftA fst . tick i) x
    , mouseFunc    = \m i x -> traverse (mouse m i) x
    , keyboardFunc = \k i x -> traverse (keyboard k i) x
    })

-- | @'runTraversableToy'@ runs any @'Traversable'@ of interactive, diagrammable
--   elements.  The first argument is the initial state, which gets run with
--   ('WithDict') the results of 'mkTraversableToyDict'.
runTraversableToy :: (Traversable t, Interactive Gtk a, CairoDiagrammable a)
                  => t a -> IO ()
runTraversableToy x = runToy $ x `WithDict` mkTraversableToyDict
