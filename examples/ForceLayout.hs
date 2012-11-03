{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeFamilies
           , TypeOperators
           , ViewPatterns
  #-}

module Toys.ForceLayout where

import Graphics.UI.Toy.Prelude

import Prelude   hiding ((.))
import Control.Category ((.))
import Data.AffineSpace.Point (Point(..))
import Data.Default
import Data.Label
import qualified Data.Map as M
import Physics.ForceLayout
import System.IO.Unsafe (unsafePerformIO)
import System.Random (newStdGen, randomRs)

data State = State (Ensemble R2) (M.Map PID (Draggable Cairo CairoDiagram)) Config

config :: State :-> Config
config = lens (\(State _ _ c) -> c) (\c (State a b _) -> State a b c)

-- TODO: add sliders to control constraints
data Config = Config
  { _dampingSlider :: CairoSlider Double
  }

$(mkLabels [''Config])

type instance V State = R2

main :: IO ()
main = runToy (def :: State)

instance Default State where
  def = State e hm $ Config $ mkSlider (0, 1) handle (0 & 100)
   where
    handle = circle 5 # lc black # lw 2

    hm = M.fromList $ [ (k, mkDraggable p handle)
                      | (k, get pos -> (P p)) <- M.toList particleMap
                      ]

    e = Ensemble [ (edges,    hookeForce 0.1 40)
                 , (allPairs, coulombForce 1)
                 ]
                 particleMap
    -- EWWW!
    entropy1 = unsafePerformIO $ newStdGen
    entropy2 = unsafePerformIO $ newStdGen
    entropy3 = unsafePerformIO $ newStdGen
    entropy4 = unsafePerformIO $ newStdGen
    count = 20
    edges       = filter (uncurry (/=))
                . take (count * 2)
                $ zip (randomRs (0, count) entropy1) (randomRs (0, count) entropy2)
    allPairs    = [(x, y) | x <- [1..count - 1], y <- [x + 1..count]]
    particleMap = M.fromList . zip [1..]
                . take count
                . map (initParticle . p2)
                $ zip (randomRs (0, 200.0) entropy3)
                      (randomRs (0, 200.0) entropy4)

uiOffset = 10 & 100

instance Interactive Gtk State where
  mouse m i s = do
    s' <- simpleMouse (\p m (State e hm c) 
                        -> State e (M.map (mouseDrag p m) hm) c) m i s
    slider <- mouse m (translate uiOffset i) $ get (dampingSlider . config) s'
    return $ set (dampingSlider . config) slider s'

  tick     = simpleTick update
  
  keyboard = handleKeys escapeKeyHandler

instance Diagrammable Cairo State where
  diagram (State e hm conf)
    = (mconcat . map diagram $ M.elems hm)
    <> translate uiOffset (diagram $ get dampingSlider conf)

instance GtkDisplay State where
  display = displayDiagram diagram

update :: State -> State
update (State e hm conf) 
  = State ( modify particles (M.intersectionWith constrain hm)
          $ ensembleStep (get (sliderValue . dampingSlider) conf) e )
          ( M.intersectionWith update hm $ get particles e )
          conf
 where
-- Move particle to its handle if dragged.
  constrain d p 
    | isDragging d = set pos (P $ get dragOffset d) p
    | otherwise    = p
-- Move non-dragging handles to their particles.
  update d (get pos -> P p)
    | isDragging d = d
    | otherwise    = set dragOffset p d
