{-# LANGUAGE FlexibleContexts
           , TypeOperators
           , TypeFamilies
           , ScopedTypeVariables
           , RankNTypes
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Utils
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Utils where

import Control.Newtype (Newtype, overF)
import Data.Label
import Data.Maybe (fromMaybe)
import Data.Basis (Basis)
import Debug.Trace (trace)
import Diagrams.Prelude hiding (trace)

-- * Diagrams Utils

-- These functions do a "setEnvelope (getEnvelope d)" in order to avoid the
-- envelope computations unecessarily unioning the superimposed thing.

underlayScaled, overlayScaled
  :: ( HasLinearMap v, InnerSpace v, AdditiveGroup v
     , Floating (Scalar v), Ord (Scalar v), Ord (Basis v), AdditiveGroup (Scalar v)
     , Semigroup m, Monoid m )
  => QDiagram b v m -> QDiagram b v m -> QDiagram b v m

underlayMatchExtents, overlayMatchExtents
  :: ( HasLinearMap v, InnerSpace v, AdditiveGroup v
     , Floating (Scalar v), Ord (Scalar v), Ord (Basis v), AdditiveGroup (Scalar v)
     , Semigroup m, Monoid m )
  => QDiagram b v m -> (v -> QDiagram b v m) -> QDiagram b v m

underlayScaled d s
  = setEnvelope (getEnvelope d)
  $ d <> boxFit (boundingBox d) s

overlayScaled d s
  = setEnvelope (getEnvelope d)
  $ boxFit (boundingBox d) s <> d

underlayMatchExtents d f
  = setEnvelope (getEnvelope d)
  $ d <> (alignTLPreserve d . f . boxExtents $ boundingBox d)

overlayMatchExtents d f
  = setEnvelope (getEnvelope d)
  $ (alignTLPreserve d . f . boxExtents $ boundingBox d) <> d


-- Returns the second diagram, aligned to the lowest corner of the first.
alignTLPreserve
  :: forall a b. ( Enveloped a, Enveloped b, Transformable b, V a ~ V b
                 , Ord (Basis (V b))
                 )
  => a -> b -> b
alignTLPreserve a b = fromMaybe b $ do
  (l_a, _) <- getCorners $ boundingBox a
  (l_b, _) <- getCorners $ boundingBox b
  return $ translate (l_b .-. l_a) b

highlight :: (PathLike (QDiagram b R2 m), Monoid m, Semigroup m)
          => Colour Double -> QDiagram b R2 m -> QDiagram b R2 m
highlight c d = underlayScaled d . fc c $ square 1

--newtype VLift = VLift (forall a. Num a => a)


-- * Normal Utils
overM :: (Monad m, Functor m, Newtype n' o', Newtype n o)
      => (o -> n) -> (o -> m o') -> n -> m n'
overM x f = (x `overF` (>>= f)) . return

modifyM :: Monad m => (b :-> a) -> (a -> m a) -> b -> m b
modifyM l f x = do
  a <- f $ get l x
  return $ set l a x

firstM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
firstM f = (\(x, y) -> f x >>= \x' -> return (x', y))

secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM f = (\(x, y) -> f y >>= \y' -> return (x, y'))

debug :: Show a => a -> a
debug x = trace (show x) x

debug' :: Show a => String -> a -> a
debug' s x = trace (s ++ show x) x
