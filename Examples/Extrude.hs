{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
           , ViewPatterns
           , MultiParamTypeClasses
  #-}
-- | Demonstrates extrude: https://github.com/mgsloan/diagrams-lib/tree/extrude
module Examples.Extrude where

import Data.Default
import Data.Label
import Data.Maybe (fromJust)

import Graphics.UI.Toy.Prelude
import Data.AffineSpace.Point (Point(..))

import Diagrams.TwoD.Combinators
import Diagrams.Combinators

--newtype State = State (Transformed (Either CairoHandle (Slider))
newtype State = State CairoHandle
  deriving (Interactive Gtk)

type instance V State = R2

main :: IO ()
main = runToy (def :: State)

instance Default State where
  def = State . set dragOffset (r2 (100, 100)) $ mkHandle 5

instance Diagrammable Cairo R2 State where
  diagram (State h)
      = translate o (runEnvelopeTest d)
     <> diagram h
     <> arrow v (8, 16) # stroke # lc black # lw 2 # translate o
    where
      d = square 200
        # rotateBy (1/8)
        # extrudeEnvelope v
      v = get dragOffset h ^-^ o
      o = (300 & 300)

instance GtkDisplay State where
  display = displayDiagram diagram

arrow :: R2               -- ^ Vector used for direction and length
      -> (Double, Double) -- ^ Width and height of the arrow head
      -> Path R2          -- ^ Path which yields an arrow diagram when stroked
arrow v (w, h) = Path
  [ (P zeroV, ltrail [ v ] False)
  , (P (v ^+^ hp ^+^ hn), ltrail [ negateV (hn ^+^ hp), hn ^-^ hp ] False)
  ]
 where
  nv = negateV $ normalized v
  hn = nv ^* h
  hp = cw (unr2 nv) ^* w
  ltrail = Trail . map Linear
  cw (x, y) = r2 (negate y, x)


runEnvelopeTest :: CairoDiagram -> CairoDiagram
runEnvelopeTest = lc black . lw 2 . sampleEnvelope2D 100

sampleEnvelope2D :: Int -> CairoDiagram -> CairoDiagram 
sampleEnvelope2D n d = foldr (flip atop) (d # lc red) bs
    where b  = fromJust $ appEnvelope (envelope d)
          bs :: [CairoDiagram]
          bs = [ stroke $ mkLine (origin .+^ (s *^ v)) (perp v)
               | v <- vs, let s = b v ]
          vs = map r2 [ (2 * cos t, 2 * sin t)
                      | i <- [0..n]
                      , let t = ((fromIntegral i) * 2.0 * pi) / (fromIntegral n)
                      ]
          mkLine a v = (moveTo a $ fromOffsets [v] # centerXY)
          perp (unr2 -> (x,y)) = r2 (-y,x) # scale 5

