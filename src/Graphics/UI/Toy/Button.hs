{-# LANGUAGE ConstraintKinds
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
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Button
  ( Button
  , buttonHeld, buttonHit, buttonFunc, buttonState
  
  -- * Convenience Newtypes
  , CairoInvertButton
  , mkInvertButton, mkCairoButton
  ) where

import Control.Newtype (unpack)
import Control.Newtype.TH
import Data.Colour.Names (black, white)
import Data.Label
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.Cairo.Text (textLineBounded)
import Diagrams.Prelude
import Graphics.Rendering.Diagrams.Points (Point(..))

import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Utils (overM, highlight, underlayMatchExtents)

-- TODO: callback rather than "buttonHit" state?

data Button b a = Button
  { _buttonHeld :: Bool  -- ^ Whether the mouse is currently pressing
  , _buttonHit  :: Bool  -- ^ Whether the button was hit
  , _buttonFunc :: Button b a -> Button b a -- ^ Transform the button state
  , _buttonState :: a    -- ^ Current state
  }

type CairoButton = Button Cairo

type instance V (Button b a) = V a

$(mkLabels [''Button])

button :: (Button b a -> Button b a) -> a -> Button b a
button = Button False False

buttonId :: a -> Button b a
buttonId = button id

--TODO: allow custom clickable as before?

instance (Diagrammable b a, V a ~ R2) => Interactive ib (Button b a) where
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
   	ci = clickInside b . P . r2 $ mousePos i
    
  mouse _ _ b = return b

instance (Diagrammable b a) => Diagrammable b (Button b a) where
  diagram = diagram . get buttonState

instance ( Diagrammable b a, v ~ V a
         , InnerSpace v, HasLinearMap v, OrderedField (Scalar v)
         ) => Enveloped (Button b a)
 where
  getEnvelope b = getEnvelope (diagram b :: Diagram b v)

instance ( Diagrammable b a, v ~ V a
         , InnerSpace v, HasLinearMap v, OrderedField (Scalar v)
         ) => Clickable (Button b a)
 where
  clickInside d = clickInside (diagram d :: Diagram b v)

newtype InvertButton b a = InvertButton (Button b a)

$(mkNewType ''InvertButton)

type instance V (InvertButton b a) = V a

type CairoInvertButton = InvertButton Cairo

-- | Creates a button from a diagrammable.  Ordinarily, the resulting diagram
--   is drawn with white fill and black stroke.  When held, the diagram is
--   instead drawn with black fill and white stroke.
mkInvertButton :: a -> InvertButton b a
mkInvertButton = InvertButton . buttonId

mkCairoButton :: Style R2 -> String -> CairoInvertButton CairoDiagram
mkCairoButton style txt
  = mkInvertButton
  . underlayMatchExtents (textLineBounded style txt)
  $ flip (uncurry roundedRect . unr2) 3

instance (Diagrammable b a, V a ~ R2)
  => Interactive ib (InvertButton b a)
 where
  mouse m i = InvertButton `overM` mouse m i

instance ( Diagrammable b a, v ~ V a
         , InnerSpace v, HasLinearMap v, OrderedField (Scalar v)
         ) => Diagrammable b (InvertButton b a) where
  diagram (InvertButton b) = styl . diagram $ get buttonState b
   where
    styl | get buttonHeld b = fc black . lc white
         | otherwise        = fc white . lc black

instance ( Diagrammable b a, v ~ V a
         , InnerSpace v, HasLinearMap v, OrderedField (Scalar v)
         ) => Enveloped (InvertButton b a)
 where
  getEnvelope b = getEnvelope (diagram b :: Diagram b v)

instance ( Diagrammable b a, v ~ V a
         , InnerSpace v, HasLinearMap v, OrderedField (Scalar v)
         ) => Clickable (InvertButton b a)
 where
  clickInside d = clickInside (diagram d :: Diagram b v)