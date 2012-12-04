{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Examples.KeyMove where

import Graphics.UI.Toy.Prelude

data State = State {
  pos, vel :: R2
}

type instance V State = R2

instance Interactive Gtk State where
  tick input state = return (State pos' vel', True)
   where
    pos' = pos state       + vel' 
    vel' = vel state * 0.9 + acc

    handleKey key v = r2 $ if keyHeld key input then v else (0, 0)

    acc = handleKey "w" ( 0, -1)
        + handleKey "a" (-1,  0)
        + handleKey "s" ( 0,  1)
        + handleKey "d" ( 1,  0)

instance Diagrammable Cairo R2 State where
  diagram state = translate (pos state) $ circle 20 # lc black # lw 2

instance GtkDisplay State where
  display = displayDiagram diagram

main = runToy $ State (100 & 100) (0 & 0)