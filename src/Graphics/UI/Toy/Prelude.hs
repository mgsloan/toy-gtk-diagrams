-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Prelude
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module re-exports the "Diagrams.Prelude", along with all of the
-- exported modules in gtk-toy-diagrams.
--
-- While importing this module should be discouraged in \"real\" code, it's
-- convenient for \"toy\" code.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Prelude
  ( module Diagrams.Prelude
  , module Graphics.UI.Toy.Gtk
  , module Graphics.UI.Toy.Button
  , module Graphics.UI.Toy.Diagrams
  , module Graphics.UI.Toy.Dict
  , module Graphics.UI.Toy.Draggable
  , module Graphics.UI.Toy.Slider
  , module Graphics.UI.Toy.Transformed
  , Cairo
  ) where

import Diagrams.Backend.Cairo ( Cairo )
import Diagrams.Prelude
import Graphics.UI.Toy.Gtk
import Graphics.UI.Toy.Button
import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Dict
import Graphics.UI.Toy.Draggable
import Graphics.UI.Toy.Slider
import Graphics.UI.Toy.Transformed
