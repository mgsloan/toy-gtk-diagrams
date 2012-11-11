{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeFamilies #-}
module Graphics.UI.Toy.Dict
  ( InteractiveDict(..), GtkDisplayDict(..), GtkInteractiveDict(..), DiagrammableDict(..)
  , WithDict(..), Dictionary(..), withDict
  , mkDisplayDict, mkSimpleInteractive
  , mkPureToy, runPureToy
  , mkMouseToy, runMouseToy
  , runSimpleToy
  , mkDiagrammableToy, runDiagrammableToy
  , mkTraversableToy, runTraversableToy
  ) where

import Control.Arrow (first)
import Data.Default
import qualified Data.Traversable as T

import Diagrams.Prelude
import Diagrams.Backend.Cairo ( Cairo )
import Graphics.UI.Gtk ( DrawWindow )
import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Diagrams


data WithDict d a = WithDict a (d a)

type instance V (WithDict d a) = V a

instance Clickable a => Clickable (WithDict d a) where
  clickInside (WithDict x _) = clickInside x

instance (Transformable a, HasLinearMap (V a))
      => Transformable (WithDict d a) where
  transform t (WithDict x d) = transform t x `WithDict` d

instance ( Enveloped a, v ~ V a, InnerSpace v, HasLinearMap v )
      => Enveloped (WithDict d a) where
  getEnvelope (WithDict x _) = getEnvelope x

instance (HasOrigin a, VectorSpace (V a))
      => HasOrigin (WithDict d a) where
  moveOriginTo p (WithDict x d) = moveOriginTo p x `WithDict` d

instance ( Enveloped a, HasOrigin a, HasLinearMap (V a))
      => Juxtaposable (WithDict d a) where
  juxtapose = juxtaposeDefault


data InteractiveDict b a = InteractiveDict
  { tickFunc     ::               InputState b -> a -> IO (a, Bool)
  , mouseFunc    :: MouseEvent -> InputState b -> a -> IO a
  , keyboardFunc :: KeyEvent   -> InputState b -> a -> IO a
  }

instance Default (InteractiveDict b a) where
  def = InteractiveDict
    { tickFunc = const $ return . (, False)
    , mouseFunc = const . const $ return
    , keyboardFunc = const . const $ return
    }

instance Interactive b (WithDict (InteractiveDict b) a) where
  tick       i (WithDict x d) = fmap (first (`WithDict` d)) $ tickFunc     d   i x
  mouse    m i (WithDict x d) = fmap (`WithDict` d)         $ mouseFunc    d m i x
  keyboard k i (WithDict x d) = fmap (`WithDict` d)         $ keyboardFunc d k i x


data GtkDisplayDict a = GtkDisplayDict
  { displayFunc :: DrawWindow -> InputState Gtk -> a -> IO a }

instance Default (GtkDisplayDict a) where
  def = GtkDisplayDict $ const $ const return

instance GtkDisplay (WithDict GtkDisplayDict a) where
  display  w i (WithDict x d) = fmap (`WithDict` d) $ displayFunc  d w i x


data GtkInteractiveDict a = GtkInteractiveDict
  { gtkDisplayDict :: GtkDisplayDict a
  , interactiveDict :: InteractiveDict Gtk a
  }

instance Default (GtkInteractiveDict a) where
  def = GtkInteractiveDict def def

instance Interactive Gtk (WithDict GtkInteractiveDict a) where
  tick       i (WithDict x d) = fmap (first (`WithDict` d)) $ tickFunc     (interactiveDict d)   i x
  mouse    m i (WithDict x d) = fmap (`WithDict` d)         $ mouseFunc    (interactiveDict d) m i x
  keyboard k i (WithDict x d) = fmap (`WithDict` d)         $ keyboardFunc (interactiveDict d) k i x

instance GtkDisplay      (WithDict GtkInteractiveDict a) where
  display  w i (WithDict x d) = fmap (`WithDict` d) $ displayFunc  (gtkDisplayDict d) w i x


data DiagrammableDict b a = DiagrammableDict
  { diagramFunc :: a -> Diagram b (V a) }

instance ( InnerSpace (V a), HasLinearMap (V a)
         , AdditiveGroup (Scalar (V a)), Ord (Scalar (V a)), Floating (Scalar (V a)) )
      => Default (DiagrammableDict b a) where
  def = DiagrammableDict $ const mempty


class Dictionary d where
  dict :: d

instance Interactive b a => Dictionary (InteractiveDict b a) where
  dict = InteractiveDict tick mouse keyboard

instance GtkDisplay a => Dictionary (GtkDisplayDict a) where
  dict = GtkDisplayDict display

instance Diagrammable b a => Dictionary (DiagrammableDict b a) where
  dict = DiagrammableDict diagram

instance (Interactive Gtk a, GtkDisplay a) => Dictionary (GtkInteractiveDict a) where
  dict = GtkInteractiveDict dict dict

withDict :: Dictionary (d a) => a -> WithDict d a
withDict x = WithDict x dict

mkDisplayDict :: (a -> CairoDiagram) -> GtkDisplayDict a
mkDisplayDict = GtkDisplayDict . displayDiagram

mkSimpleInteractive
  :: (              InputState b -> a -> (a, Bool))
  -> (MouseEvent -> MousePos   b -> a -> a)
  -> (KeyEvent                   -> a -> a)
  -> InteractiveDict b a
mkSimpleInteractive tf mf kf = InteractiveDict
  (\i -> return . tf i)
  (simpleMouse mf)
  (simpleKeyboard kf)

mkPureToy
  :: (a -> CairoDiagram)
  -> (              InputState Gtk -> a -> (a, Bool))
  -> (MouseEvent -> MousePos   Gtk -> a -> a)
  -> (KeyEvent                     -> a -> a)
  -> GtkInteractiveDict a
mkPureToy df tf mf kf = GtkInteractiveDict
  (mkDisplayDict df)
  (mkSimpleInteractive tf mf kf)

runPureToy
  :: (a -> CairoDiagram)
  -> (              InputState Gtk -> a -> (a, Bool))
  -> (MouseEvent -> MousePos   Gtk -> a -> a)
  -> (KeyEvent                     -> a -> a)
  -> a -> IO ()
runPureToy df tf mf kf x = runToy $ x `WithDict` mkPureToy df tf mf kf

mkMouseToy
  :: (a -> CairoDiagram)
  -> (Bool -> MousePos Gtk -> a -> a)
  -> GtkInteractiveDict a
mkMouseToy df mf = GtkInteractiveDict
  (mkDisplayDict df)
  (InteractiveDict
    (\_ x -> return (x, False))
    (\_ i x -> return $ mf (keyHeld "Mouse1" i || keyHeld "Mouse2" i) (mousePos i) x)
    (\_ _ x -> return x))

runMouseToy
  :: (a -> CairoDiagram)
  -> (Bool -> MousePos Gtk -> a -> a)
  -> a -> IO ()
runMouseToy df mf x = runToy $ x `WithDict` mkMouseToy df mf

runSimpleToy :: (MousePos Gtk -> CairoDiagram) -> IO ()
runSimpleToy f = runMouseToy f (\_ p _ -> p) (0, 0)

-- Used to be the newtype \"TDia\"
mkDiagrammableToy :: (Interactive Gtk a, Diagrammable Cairo a, V a ~ R2)
                  => a -> WithDict GtkInteractiveDict a
mkDiagrammableToy x = x `WithDict` GtkInteractiveDict (mkDisplayDict $ diagramFunc dict) dict

runDiagrammableToy :: (Interactive Gtk a, Diagrammable Cairo a, V a ~ R2)
                   => a -> IO ()
runDiagrammableToy = runToy . mkDiagrammableToy

mkTraversableToy :: (T.Traversable t, Interactive Gtk a, Diagrammable Cairo a, V a ~ R2)
                 => t a -> WithDict GtkInteractiveDict (t a)
mkTraversableToy t = WithDict t $ GtkInteractiveDict
  (mkDisplayDict $ T.foldMapDefault (diagramFunc dict))
  (InteractiveDict
  -- TODO: or together the boolean results
    { tickFunc     = \  i x -> liftA (, True) $ T.traverse (liftA fst . tick i) x
    , mouseFunc    = \m i x -> T.traverse (mouse m i) x
    , keyboardFunc = \k i x -> T.traverse (keyboard k i) x
    })

runTraversableToy :: (T.Traversable t, Interactive Gtk a, Diagrammable Cairo a, V a ~ R2)
                 => t a -> IO ()
runTraversableToy = runToy . mkTraversableToy