-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Gtk.Text
-- Copyright   :  (c) 2013 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Utilities for drawing text.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Gtk.Text
  ( preText
  ) where

import Diagrams.Backend.Cairo.Text
import Diagrams.Prelude
import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Gtk.Diagrams

-- | Use cairo to render bounded monospace text, handling newlines properly.
preText :: String -> CairoDiagram
preText = vcat . map (textLineBounded monoStyle) . lines
