-- |
-- Module      :  Graphics.UI.Toy.Gtk.Text
-- Copyright   :  (c) 2013 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Text utilities.
module Graphics.UI.Toy.Gtk.Text
  ( preText
  ) where

import Diagrams.Backend.Cairo.Text
import Diagrams.Prelude
import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Gtk.Diagrams

preText :: String -> CairoDiagram
preText = vcat . map (textLineBounded monoStyle) . lines
