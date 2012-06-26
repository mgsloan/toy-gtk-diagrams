{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
-- Ported from gloss machinima
import Graphics.UI.Toy.Prelude
import Data.Colour.SRGB (sRGB)
import Data.Default
import Animator

data Machinima = Machinima Double

type instance V Machinima = R2

main :: IO ()
main = runToy $ def { animDiagram = animMach }
 where
  animMach t
    = mach t 6
    # rotate (Deg $ t * 30)
    # scale 0.8

  mach t 0 = leaf
  mach t d
    = mconcat
    [ leaf
    , rec 
      # rotate    (Deg $ 90 + t * 30)
      # translate (r2 (0, -100))
    , rec
      # rotate    (Deg $ 90 - t * 30)
      # scale     0.8
      # translate (r2 (0, 100))
    ]
   where
    rec = mach (t * 1.5) (d - 1)

  loop = [(-10, -100), (-10, 100), (10, 100), (10, -100), (-10, -100)]

  leaf = fromVertices (map p2 loop)
       # fillColor (sRGB 1.0 1.0 1.0 `withOpacity` 0.5)
       # lineColor (sRGB 0.0 0.0 1.0 `withOpacity` 0.8)