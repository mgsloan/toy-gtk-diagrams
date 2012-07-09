{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

import Graphics.UI.Toy.Prelude

data Sprite = Sprite {
  pos, vel :: R2
}

type instance V Sprite = R2

instance Interactive Gtk Sprite where
  tick input state = return (Sprite pos' vel', True)
   where
    pos' = pos state       + vel' 
    vel' = vel state * 0.9 + acc

    handleKey key v = r2 $ if keyHeld key input then v else (0, 0)

    acc = handleKey "w" ( 0, -1)
        + handleKey "a" (-1,  0)
        + handleKey "s" ( 0,  1)
        + handleKey "d" ( 1,  0)

instance Diagrammable Cairo Sprite where
  diagram state = translate (pos state) $ circle 20

instance GtkDisplay Sprite where
  display = displayDiagram diagram

main = runToy $ Sprite (r2 (100,100)) (r2 (0, 0))