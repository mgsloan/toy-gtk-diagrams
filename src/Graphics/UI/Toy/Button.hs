{-# LANGUAGE 
   ConstraintKinds
 , FlexibleInstances
 , FlexibleContexts
 , GeneralizedNewtypeDeriving
 , MultiParamTypeClasses
 , ScopedTypeVariables
 , TemplateHaskell
 , TypeFamilies
 , TypeOperators
 , TypeSynonymInstances
 , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Button
-- Copyright   :  (c) 2011 Michael Sloan (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
-- Simple button UI element.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Button
  ( ButtonState(..), Button(..), CairoButton
 
  -- * Lenses
  , buttonState, buttonHit, buttonDiagram

  -- * Mutation
  , clearButtonHit

  -- * Construction
  , mkButton, mkDefaultButton
  ) where

import Control.Newtype (Newtype, pack)
import Data.AffineSpace.Point (Point(P))
import Data.Colour
import Data.Colour.Names
import Data.Label
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.Cairo.Text (textLineBounded)
import Diagrams.Prelude

import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Diagrams

type CairoButton = Button Cairo R2

data ButtonState
  = NormalState
  | HoverState
  | PressState
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

-- | A button stores the state necessary to know if the button is currently
--   being pressed ('_buttonHeld'), and if it was hit ('_buttonHit').  The
--   semantics of '_buttonHit' are up to the user, as it's up to the user to
--   call 'clearButtonHit' or otherwise set its value to @False@.
--
--   In order to draw the button, and figure out when mouse-clicks are inside
--   it, the function '_buttonDiagram' is used. It draws the button based on
--   the current '_buttonHeld' state.
data Button b v = Button
  { _buttonState   :: ButtonState -- ^ Whether the mouse is hovering / pressing.
  , _buttonHit     :: Bool        -- ^ Whether the button was hit.
  , _buttonDiagram :: Button b v -> Diagram b v 
                                  -- ^ Draw button based on the state.
  }

type instance V (Button b v) = v

$(mkLabels [''Button])

-- | Builds a button, given the function used to draw it.  The first argument
--   of this function is a boolean that indicates whether it's held.
mkButton :: (Button b v -> Diagram b v) -> Button b v
mkButton = Button NormalState False

-- | Builds a button containing text.  The outside border is a rounded
--   rectangle, and when pressed, it's drawn with a black fill and white lines.
mkDefaultButton :: String -> CairoButton
mkDefaultButton txt = mkButton dia
  where
    dia b = addTint $ case _buttonState b of
        NormalState -> blackLined $ label <> border
        HoverState  -> blackLined $ label <> fc lightgray border
        PressState  -> fc white label <> (border # fc black # lc white)
      where
        addTint
          | _buttonHit b = flip overlayScaled (square 1 # fcA redTint)
          | otherwise    = id
        redTint = red `withOpacity` 0.5
    border = centerXY . lw 2 $ roundedRect (width label + 5) (height label + 5) 3
    label = centerXY . pad 1 . reflectY $ textLineBounded monoStyle txt

-- | This function literally just 'set's 'buttonHit' to 'False'.
clearButtonHit :: Button b v -> Button b v
clearButtonHit = set buttonHit False

instance ( Newtype v (MousePos ib)
         , HasLinearMap v, InnerSpace v, OrderedField (Scalar v) )
      => Interactive ib (Button b v) where
  mouse m i b = return $ transition b
   where
    ci = clickInside b . P . pack $ mousePos i
    transition = case (ci, m, _buttonState b) of
      (True, Just (True,  0),          _) -> set buttonState PressState
      (True, Just (False, 0), PressState) -> set buttonState HoverState . set buttonHit True
      (True,               _, PressState) -> id
      (True,               _,          _) -> set buttonState HoverState
      (False,              _,          _) -> set buttonState NormalState

instance Diagrammable b v (Button b v) where
  diagram x = get buttonDiagram x x

instance (InnerSpace v, HasLinearMap v, OrderedField (Scalar v))
      => Enveloped (Button b v) where
  getEnvelope b = getEnvelope (diagram b :: Diagram b v)

instance (InnerSpace v, HasLinearMap v, OrderedField (Scalar v))
      => Clickable (Button b v) where
  clickInside b = clickInside (diagram b :: Diagram b v)