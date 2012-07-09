{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

import Graphics.UI.Toy.Prelude

data Sprite = Sprite {
  pos, vel :: R2
} deriving Show

type instance V Sprite = R2

instance Interactive Gtk Sprite where
  tick inp s = return (Sprite pos' vel', True)
   where
    pos' = pos s       + vel' 
    vel' = vel s * 0.9 + accel

    handleKey c v = r2 $ if keyHeld c inp then v else (0, 0)

    accel = handleKey "w" ( 0, -1)
          + handleKey "a" (-1,  0)
          + handleKey "s" ( 0,  1)
          + handleKey "d" ( 1,  0)

instance Diagrammable Cairo Sprite where
  diagram s = translate (pos s) $ circle 20

instance GtkDisplay Sprite where
  display = displayDiagram diagram

main = runToy $ Sprite (r2 (100,100)) (r2 (0, 0))