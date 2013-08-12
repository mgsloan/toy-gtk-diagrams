{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeFamilies
  , TypeSynonymInstances
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Gtk.Diagrams
-- Copyright   :  (c) 2013 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- toy-diagrams utilities specific to the toy-gtk backend.
--
--------------------------------------------------------------------------------
module Graphics.UI.Toy.Gtk.Diagrams
  (
  -- * Convenient Type Synonyms
    CairoDiagram, CairoDiagrammable, CairoInteractive
  , CairoButton
  , CairoDraggable, CairoHandle
--  , CairoSlider
  -- * Displaying diagrams
  , defaultDisplay
  , displayDiagram
  -- * Widgets
  , mkDefaultButton
  ) where

import Control.Lens         hiding ( transform, (#) )
import Diagrams.Backend.Cairo      ( Cairo )
import Diagrams.Backend.Cairo.Text ( textLineBounded )
import Diagrams.Backend.Gtk        ( renderToGtk )
import Diagrams.Prelude
import Diagrams.Lens

import Graphics.UI.Gtk             ( DrawWindow )
import Graphics.UI.Toy.Gtk         ( Gtk )
import Graphics.UI.Toy
import Graphics.UI.Toy.Button
import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Draggable
-- import Graphics.UI.Toy.Slider

type CairoDiagram = Diagram Cairo R2
type CairoDiagrammable q a = Diagrammable Cairo R2 q a
type CairoInteractive a = (Diagrammable Cairo R2 Any a, Interactive Gtk a)

type CairoButton = Button Cairo R2
type CairoDraggable a = Draggable a
type CairoHandle = Draggable CairoDiagram

-- type CairoSlider a = Slider Cairo R2 a
-- | Convenience function for implementing the display function of 'GtkDisplay'.
displayDiagram :: (a -> CairoDiagram)
               -> DrawWindow -> InputState Gtk -> a -> IO a
displayDiagram f dw _ x = (renderToGtk dw $ f x) >> return x

-- | Simply @'displayDiagram' 'diagram'@, useful for boilerplate implementations
--   of 'GtkDisplay'.
defaultDisplay :: CairoDiagrammable Any a
               => DrawWindow -> InputState Gtk -> a -> IO a
defaultDisplay = displayDiagram diagram


type instance V (InputState Gtk) = R2

instance Transformable (InputState Gtk) where
  transform t is = is { mousePos = (wrapped . _P  %~ transform (inv t))
                                 $ mousePos is }

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


-- TODO: reinstate when it's figured out how to not use "UndecidableInstances"
-- TODO: GtkDisplay Button instance
-- TODO: document orphans
-- TODO: can these work for any q?

{-
instance (V a ~ R2, CairoDiagrammable Any a) => GtkDisplay (CairoDraggable a) where
  display = displayDiagram diagram

instance CairoDiagrammable Any a => GtkDisplay (CairoSlider a) where
  display = displayDiagram diagram

instance (V a ~ R2, CairoDiagrammable Any a) => GtkDisplay (Transformed a) where
  display = displayDiagram diagram
-}
