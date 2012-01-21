module Graphics.UI.Gtk.Toy.Text.Interactive where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Text

import Control.Arrow (first, second)

import Data.Colour.Names (black, yellow)
import Data.Maybe (maybeToList)
import Diagrams.Prelude
import Diagrams.Backend.Cairo.Text (StyleParam, textLineBounded)

import System.IO.Unsafe

initialState :: (Mark m, CanBeCursor m) => MarkedText m
initialState = MarkedText "" [((0, 0), mkCursor)]

instance (Eq m, Show m, Mark m, CanBeCursor m)
      => Interactive (MarkedText m) where
  keyboard = simpleKeyboard handleKey
  display  = displayDiagram (scaleY (-1) . margin . monoText) 

margin :: CairoDiagram -> CairoDiagram
margin = (strutX 10 |||) . (strutY 18 ===)

monoText :: Mark m => MarkedText m -> CairoDiagram
monoText = drawText (font "monospace" . fontSize 18)

handleKey ::(Eq m, Mark m, CanBeCursor m)
          => KeyEvent -> MarkedText m -> MarkedText m 
handleKey (True, e) mt = case e of
  Right k -> insert [k]
  Left  k -> case k of
    "Return"    -> insert "\n"
    "Left"      -> mutateCursors (subtract 1)
    "Right"     -> mutateCursors (+1)
    "Home"      -> mutateCursors (const (-1, -1))
    "End"       -> mutateCursors (const (maxIx, maxIx))
    "Delete"    -> editCursors (\(ivl, _) -> (second  (+1)  ivl, initialState))
    "BackSpace" -> editCursors (\(ivl, _) -> (first (+(-1)) ivl, initialState))
    "Escape"    -> unsafePerformIO $ (quitToy >> return mt)
    _           -> mt
 where
  editCursors f = edit (whenMarked isCursor f) mt
  insert s = editCursors $ second $ const (MarkedText s [((p, p), mkCursor)])
    where p = length s
  mutateCursors f = clipMarks . editCursors . second . mutateMarks
                  $ \m -> first f <$> toMaybe (isCursor . snd) m
  maxIx = textLength mt

handleKey _ ts = ts

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f x = if f x then Just x else Nothing