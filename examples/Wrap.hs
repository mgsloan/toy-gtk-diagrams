{-# LANGUAGE DeriveDataTypeable
           , TemplateHaskell
           , TypeFamilies
           , TypeSynonymInstances
           , FlexibleInstances
           , MultiParamTypeClasses 
           , FlexibleContexts
           , TypeOperators #-}

module Toys.Wrap where

import Graphics.UI.Toy.Prelude

import Control.Applicative ((<$>))
import Control.Category ((.))
import Prelude hiding ((.))
import Data.Data (Data, Typeable)
import Data.Default
import Data.Label
import Data.List (intersperse)
import Diagrams.Prelude hiding (text)
import Diagrams.Backend.Cairo
import Diagrams.Layout.Wrap

type MTC = MarkedText CursorMark

data State = State { _txt :: MTC
                   , _bnds :: Draggable Cairo (Diagram Cairo R2)
                   }

type instance V State = R2

$(mkLabels [''State])

main :: IO ()
main = runToy (def :: State)

loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

instance Default State where
  def = State (addText cursorText (plainText loremIpsum) :: MTC)
              (mkDraggable (r2 (50, 50)) $ circle 200)

instance Diagrammable Cairo State where
  diagram s = r <> diaBnds
   where
    diaBnds = diagram (get bnds s)
    axis = [unitX, negateV $ unitY]
    r = wrapDiagram
      . wrapInside (getAny . sample diaBnds) axis (p2 (10, 10))
      . intersperse (strutX 10)	
      . map (alignTL . diagram . (plainText :: String -> MTC)) . words
      $ get (mText . txt) s

instance Interactive Gtk State where
  keyboard = simpleKeyboard (\k -> modify txt (textKeyHandler k))
  mouse m i = modF bnds (mouse m $ flipMouse i)

instance GtkDisplay State where
  display = displayDiagram (scaleY (-1) . diagram)

flipMouse :: InputState Gtk -> InputState Gtk
flipMouse (InputState (x, y) kt) = InputState (x, -y) kt

modF :: (Functor f) => (b :-> a) -> (a -> f a) -> b -> f b
modF l f s = flip (set l) s <$> f (get l s)
