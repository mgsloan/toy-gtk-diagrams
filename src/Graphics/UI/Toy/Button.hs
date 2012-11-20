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
  ( Button(..)
 
  -- * Lenses
  , buttonHeld, buttonHit, buttonDiagram

  -- * Mutation
  , clearButtonHit

  -- * Construction
  , mkButton, mkDefaultButton
  ) where

import Control.Newtype (Newtype, pack)
import Data.AffineSpace.Point (Point(P))
import Data.Label
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.Cairo.Text (textLineBounded)
import Diagrams.Prelude

import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Diagrams

type CairoButton = Button Cairo R2

-- | A button stores the state necessary to know if the button is currently
--   being pressed ('_buttonHeld'), and if it was hit ('_buttonHit').  The
--   semantics of '_buttonHit' are up to the user, as it's up to the user to
--   call 'clearButtonHit' or otherwise set its value to @False@.
--
--   In order to draw the button, and figure out when mouse-clicks are inside
--   it, the function '_buttonDiagram' is used. It draws the button based on
--   the current '_buttonHeld' state.
data Button b v = Button
  { _buttonHeld :: Bool                   -- ^ Whether the mouse is currently pressing
  , _buttonHit  :: Bool                   -- ^ Whether the button was hit
  , _buttonDiagram :: Bool -> Diagram b v -- ^ Draw button based on '_buttonHeld' 
  }

type instance V (Button b v) = v

$(mkLabels [''Button])

-- | Builds a button, given the function used to draw it.  The first argument
--   of this function is a boolean that indicates whether it's held.
mkButton :: (Bool -> Diagram b v) -> Button b v
mkButton = Button False False

-- | Builds a button containing text.  The outside border is a rounded
--   rectangle, and when pressed, it is drawn with a black fill and white lines.
mkDefaultButton :: String -> CairoButton
mkDefaultButton txt = mkButton f
  where
    f True = fc black . lc white $ lw 2 dia
    f False = blackLined dia
    dia = centerXY label <> centerXY (roundedRect (width label + 5) (height label + 5) 3)
    label = pad 1 . reflectY $ textLineBounded monoStyle txt

-- | This function literaly just 'set's 'buttonHit' to 'False'.
clearButtonHit :: Button b v -> Button b v
clearButtonHit = set buttonHit False

instance ( Newtype v (MousePos ib)
         , HasLinearMap v, InnerSpace v, OrderedField (Scalar v) )
      => Interactive ib (Button b v) where
  mouse (Just (c, 0)) i b
    | ci && c   = return
                . set buttonHeld True
                $ b
    | not   c   = return
                . set buttonHeld False 
                . set buttonHit  (ci && get buttonHeld b)
                $ b
    | otherwise = return b
   where
    ci = clickInside b . P . pack $ mousePos i
    
  mouse _ _ b = return b

instance Diagrammable b (Button b v) where
  diagram x = get buttonDiagram x $ get buttonHeld x

instance ( InnerSpace v, HasLinearMap v, OrderedField (Scalar v)
         ) => Enveloped (Button b v)
 where
  getEnvelope b = getEnvelope (diagram b :: Diagram b v)

instance ( InnerSpace v, HasLinearMap v, OrderedField (Scalar v)
         ) => Clickable (Button b v)
 where
  clickInside b = clickInside (diagram b :: Diagram b v)