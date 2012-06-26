{-# LANGUAGE ConstraintKinds
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
  -- * CairoDiagram type alias
    CairoDiagram

  -- * Diagrammable class
  , Diagrammable(..)
  , CairoDiagrammable
  , Clickable(..)
  
  -- * Traversable Default Wrappers
  , TToy(..), TDia(..)

  , displayDiagram
  ) where

import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Utils

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Gtk

import Control.Applicative ((<$>), pure)
import Control.Arrow ((***))
import Control.Newtype (Newtype, pack, unpack)
import Control.Newtype.TH
import Data.Label
import qualified Data.Traversable as T
import qualified Graphics.UI.Gtk as G

type CairoDiagram = Diagram Cairo R2

class Diagrammable b a where
  diagram :: a -> Diagram b (V a)

type CairoDiagrammable = Diagrammable Cairo

instance Diagrammable b (Diagram b v) where
  diagram = id

-- | Clickable things have some concept of which positions are clickable.
class Clickable a where
  clickInside :: a -> Point (V a) -> Bool

instance HasLinearMap v => Clickable (Diagram b v) where
  clickInside d = getAny . runQuery (query d)

--TODO: rather than newtypes, consider having a few "Prototype" wrappers?
-- could be the appropriate response to newtype-anti-pattern.

-- | Wrapper for making traversable things interactive.
newtype TToy t a = TToy (t a)

-- | Wrapper to make @GtkDisplay@ instances for Diagrammables.
newtype TDia a = TDia a
  deriving (Clickable, Juxtaposable)

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

$(mkNewTypes [''TToy, ''TDia])

-- | Convenience function for implementing the display function of
--   Interactive ib.
displayDiagram :: (a -> CairoDiagram)
               -> G.DrawWindow -> InputState Gtk -> a -> IO a
displayDiagram f dw _ x = (renderToGtk dw $ f x) >> return x

-- Traversable Toy instances

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


-- | Wrapper for making a single-item interactive instance
{-
data TProxy a b = TProxy (a :-> b) a

instance T.Traversable (TProxy a) where
  traverse f (TProxy l x) = TProxy . modify l f <$> pure x
-}