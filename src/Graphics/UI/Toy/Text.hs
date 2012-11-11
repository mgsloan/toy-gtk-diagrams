{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances
           , MultiParamTypeClasses
           , RankNTypes
           , TemplateHaskell
           , TupleSections
           , TypeFamilies
           , TypeSynonymInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Toy.Text
-- Copyright   :  (c) 2011 Michael Sloan (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
-- Provides a (currently inefficient) representation of text with styling and
-- meta-data annotations applied to particular intervals.
--
-----------------------------------------------------------------------------
module Graphics.UI.Toy.Text
  ( MarkedText(..), Ivl
  , mText, mMarks
  , emptyText, plainText
  , textLength, getMarks, smallestEnclosing
  , substrText, addText
  , applyEdit, applyEdits, edit, whenMarked, mutateSlice
  , addMark, addMarks, removeMark, mutateMarks, filterMarks, clipMarks, clearMarks
  , drawText

  , StyleState(..), monoStyle
  , CanBeCursor(..), cursorText, moveCursor
  , Mark(..), EmptyMark(..), CursorMark(..), SizeMark(..), SlantMark(..), WeightMark(..)
  ) where

import Diagrams.Prelude hiding ( trace )
import Diagrams.TwoD.Text
import Diagrams.Backend.Cairo ( Cairo )
import Diagrams.Backend.Cairo.Text ( textLineBounded )
import Control.Arrow       ( first, second, (***), (&&&) )
import Control.Monad       ( msum )
import Data.Data           ( Data, Typeable )
import Data.Either         ( partitionEithers )
import Data.Label
import Data.List           ( partition, findIndices, sortBy, sort, delete, group)
import Data.Maybe          ( catMaybes, mapMaybe, listToMaybe )
import Data.Ord            ( comparing )

import Graphics.UI.Toy.Diagrams
import Graphics.UI.Toy.Utils ( highlight )
import Graphics.UI.Toy.Gtk

import System.IO.Unsafe

type Ivl = (Int, Int)

data MarkedText m = MarkedText
  { _mText  :: String
  , _mMarks :: [(Ivl, m)]
  } deriving (Eq, Show, Typeable, Data)

type instance V (MarkedText m) = R2

$(mkLabels [''MarkedText])

newtype StyleState = StyleState (Style R2)

class CanBeCursor a where
  mkCursor :: a
  isCursor :: a -> Bool

-- TODO: function to handle mark splitting

class Mark a where
  type DrawState a
  initialDrawState :: MarkedText a -> DrawState a

  drawStateStyle :: MarkedText a -> DrawState a -> Style R2
  drawStateStyle _ _ = monoStyle

  drawMark :: a -> DrawState a -> MarkedText a -> CairoDiagram
  drawMark _ s m = drawRec s m

  mergeMark :: a -> a -> Maybe a
  mergeMark _ _ = Nothing

  splitMark :: Int -> a -> (Maybe a, Maybe a)
  splitMark _ x = (Just x, Just x)

monoStyle :: Style R2
monoStyle = font "monospace" $ fontSize 18 mempty

emptyText :: MarkedText m
emptyText = MarkedText "" []

plainText :: String -> MarkedText m
plainText t  = MarkedText t []


--TODO: use splitMark

-- | Extract an interval of the text.  First parameter is True if inclusive.
substrText :: Bool -> MarkedText m -> Ivl -> MarkedText m
substrText inc (MarkedText str ms) ivl@(f, t)
  = MarkedText (take count $ drop f' str)
  . catMaybes $ map (firstA local) ms
 where
  f' = max 0 f
  count = t - f'
-- Transform intervals into the result, yielding Nothing if they aren't
-- inside the substring interval.
  local x | x == ivl = Just (0, count)
          | inc && fst x ==  0 && fst ivl ==  0 = Just (0, min (snd x) (snd ivl))
          | inc && snd x == tl && snd ivl == tl = Just (max (fst x) (fst ivl), tl)
          | otherwise = mapT (subtract f') <$> ivlIntersect ivl x
  tl = length str

-- Sort marks, big first.
sortMarks :: [(Ivl, m)] -> [(Ivl, m)]
sortMarks = sortBy (comparing (\((f, t), _) -> (f, f - t)))

-- | Concatenates two marked texts together, attempting to merge the marks
--   incident on the join.
addText :: forall m. (Mark m, Eq m) 
      => MarkedText m -> MarkedText m -> MarkedText m
addText (MarkedText at ams) (MarkedText bt bms)
  = MarkedText (at ++ bt) rms
 where
  al = length at

-- Separate off marks that cross the border between the texts.
  (ap, an) = partition ((>=al - 1) . snd . fst) ams

  (bp, bn) = mapT (map $ first $ mapT (+al))
           $ partition ((<=0)  . fst . fst) bms

-- Collect all of the resulting marks.
  rms = sortMarks $ an ++ performMerges ap bp ++ bn

-- Merge overlapping marks from the two texts.
performMerges :: (Eq m, Mark m) => [(Ivl, m)] -> [(Ivl, m)] -> [(Ivl, m)]
performMerges []     ys = ys
performMerges (x:xs) ys = case msum . map doMerge $ ys of
  Just (y, x') -> x' : performMerges xs (delete y ys)
  Nothing      -> x  : performMerges xs ys
 where
  doMerge y = do
    i <- ivlMaybeUnion (fst x) (fst y)
    m <- mergeMark     (snd x) (snd y)
    return (y, (i, m))

--TODO: consider a mark for lines / line #s?
--TODO: figure out how style will be applied to the text by marks

instance Mark m => Diagrammable Cairo (MarkedText m) where
  diagram mt = drawText (initialDrawState mt) mt

drawText :: Mark m => DrawState m -> MarkedText m -> CairoDiagram
drawText initial mt
  = vcat
  . map (drawRec initial . substrText True sorted . second (+1))
  . ivlsFromSlices (textLength mt)
  . findIndices (=='\n')
  $ get mText mt
 where
  sorted = modify mMarks sortMarks mt

drawRec :: forall m. Mark m => DrawState m -> MarkedText m -> CairoDiagram
drawRec st mt@(MarkedText txt [])
  = textLineBounded (drawStateStyle mt st) $ filter (not . (`elem` "\r\n")) txt
drawRec st mt@(MarkedText txt (((fm, tm), m):_))
  =   drawRec st (substrText False mt (-1, fm))
  ||| drawMark m st (substrText True mt (fm, tm))
  ||| drawRec st (substrText False mt (tm, length txt + 1))

-- | A chunk specifies a located marked text.
type Chunk m = (Ivl, MarkedText m)

-- | Breaks the marked text into chunks such that the marks stay consistent
--   throughout.
textChunks :: MarkedText m -> [Chunk m]
textChunks mt
  = map (id &&& (substrText False mt))
  . ivlsFromSlices (textLength mt)
  . concatMap (ivlSlices . fst)
  $ get mMarks mt

-- | Given a list of slice points, and an overall length, yields a list of
--   intervals broken at those points.
ivlsFromSlices :: (Num a, Ord a) => a -> [a] -> [(a, a)]
ivlsFromSlices l xs = map head . group $ zip (0 : ys) (ys ++ [l])
  where ys = sort xs

textLength :: MarkedText m -> Int
textLength = length . get mText

applyEdit :: (Eq m, Mark m) => Chunk m -> MarkedText m -> MarkedText m
applyEdit ((f, t), sub) mt
  = addText (substrText False mt (-1, f))
  $ addText sub
  $ substrText False mt (t, textLength mt + 1)

-- | Applies a set of edits in such a way that the indexing is consistent.
--   If the edits overlap, then strange things happen.
applyEdits :: (Eq m, Mark m) => [Chunk m] -> MarkedText m -> MarkedText m
applyEdits edits mt
  = foldr applyEdit mt
  $ sortBy (\l -> flipOrd . comparing fst l) edits

-- | Enumerates all of the chunks of the text. The function provided as the
--   first argument is applied to each, yielding a set of edits.
edit :: (Eq m, Mark m) => (Chunk m -> [Chunk m])
     -> MarkedText m -> MarkedText m
edit f mt = (`applyEdits` mt) . concatMap f $ textChunks mt

--   the second function when the first one matches a particular mark.
whenMarked :: (a -> Bool) -> (Chunk a -> b) -> Chunk a -> [b]
whenMarked f g x@(_, MarkedText _ ms)
  | any (f . snd) ms = [g x]
  | otherwise = []

-- | Mutates a given interval of the text.
mutateSlice :: (Eq m, Mark m)
            => (MarkedText m -> MarkedText m) -> Ivl
            -> MarkedText m -> MarkedText m
mutateSlice f i mt = applyEdit (i, f $ substrText True mt i) mt

-- TODO: make more efficient - should be able to avoid slicing text.
-- | Applies a mark to the given interval.
addMark :: (Eq m, Mark m)
        => (Ivl, m) -> MarkedText m -> MarkedText m
addMark m (MarkedText txt ms) = MarkedText txt $ m : ms
{- TODO: fix
addMark (ivl, m) = mutateSlice 
  (\(MarkedText txt ms) -> MarkedText txt $ (ivl, m) : ms) ivl
-}

addMarks :: (Eq m, Mark m) => [(Ivl, m)] -> MarkedText m -> MarkedText m
addMarks ms t = foldr addMark t ms

-- | Removes marks that match the given predicate.
removeMark :: ((Ivl, m) -> Bool) -> MarkedText m -> MarkedText m
removeMark f (MarkedText txt xs) = MarkedText txt $ filter (not . f) xs

mutateMarks :: (Eq m, Mark m)
            => ((Ivl, m) -> Maybe (Ivl, m)) -> MarkedText m -> MarkedText m
mutateMarks f (MarkedText t ms) = MarkedText t $ mapMaybe f ms
{- Old defn - Nothing indicates noop)
mutateMarks f (MarkedText t ms) = MarkedText t $ ms' ++ (ms \\ del)
 where
  (del, ms') = unzip . catMaybes $ map (raiseSndA . (id &&& f)) ms
-}

filterMarks :: (Eq m, Mark m)
            => ((Ivl, m) -> Bool) -> MarkedText m -> MarkedText m
filterMarks f = mutateMarks (\m -> if f m then Just m else Nothing)

moveCursor :: CanBeCursor m => (t -> t) -> (t, m) -> (t, m)
moveCursor f (i, x) 
  | isCursor x = (f i, x)
  | otherwise = (i, x)

-- | Crops and merge marks that extend beyond the envelope of the MarkedText.
--TODO: merge behaviour
clipMarks :: (Eq m, Mark m) => MarkedText m -> MarkedText m
clipMarks mt = modify mMarks (uncurry performMerges . partitionEithers . map process) mt
 where
  l = textLength mt
  process ((f, t), m)
    | f < 0 || t < 0 || f > l || t > l = Left ((wrap f, wrap t), m)
    | otherwise = Right ((f, t), m)
  wrap = max 0 . min (textLength mt)

clearMarks :: MarkedText m -> MarkedText m
clearMarks (MarkedText t _) = MarkedText t []

getMarks :: ((Ivl, m) -> Bool) -> MarkedText m -> [(Ivl, m)]
getMarks f = filter f . get mMarks

smallestEnclosing :: (m -> Bool) -> Ivl -> MarkedText m -> Maybe (Ivl, m)
smallestEnclosing f ivl = listToMaybe
                        . sortBy (comparing $ uncurry subtract . fst)
                        . getMarks (\(i, m) -> ivlContainsIvl i ivl && f m)

-- Builtin Marks

data EmptyMark = EmptyMark deriving (Eq, Show, Data, Typeable)

instance Mark EmptyMark where
  type DrawState EmptyMark = ()
  initialDrawState _ = ()
  drawStateStyle _ _ = monoStyle
  drawMark _ _ mt = drawRec () mt
  mergeMark _ _ = Just EmptyMark

data CursorMark = CursorMark deriving (Eq, Show, Data, Typeable)

instance CanBeCursor CursorMark where
  mkCursor = CursorMark
  isCursor = const True

instance Mark CursorMark where
  type DrawState CursorMark = Style R2
  initialDrawState _ = monoStyle
  drawStateStyle _ s = s
  drawMark _ s mt@(MarkedText txt _)
    | null txt = lineWidth 1 . lineColor black
               . moveOriginBy (-1.5 & 2.0)
               . setEnvelope mempty
               . stroke . pathFromTrail
               $ Trail [Linear $ 0 & 18] False
    | otherwise = highlight black $ drawRec (applyStyle (fc white mempty) s) mt 
  mergeMark _ _ = Just CursorMark

--TODO: These don't quite work the way that they should yet, in the case that
--the user-provided style overrides them. Plus they won't effect font dimension

data SizeMark = SizeMark Double deriving (Eq, Show, Data, Typeable)
instance Mark SizeMark where
  type DrawState SizeMark = Style R2
  initialDrawState _ = monoStyle
  drawStateStyle _ s = s
  drawMark (SizeMark size) s mt
    = drawRec (applyStyle (fontSize size mempty) s) mt
  
data SlantMark = SlantMark FontSlant
instance Mark SlantMark where
  type DrawState SlantMark = Style R2
  initialDrawState _ = monoStyle
  drawStateStyle _ s = s
  drawMark (SlantMark slant) s mt
    = drawRec (applyStyle (fontSlant slant mempty) s) mt

data WeightMark = WeightMark FontWeight
instance Mark WeightMark where
  type DrawState WeightMark = Style R2
  initialDrawState _ = monoStyle
  drawStateStyle _ s = s
  drawMark (WeightMark weight) s mt
    = drawRec (applyStyle (fontWeight weight mempty) s) mt

-------------------------------------------------------------------------------
-- Interactive
-------------------------------------------------------------------------------

cursorText :: (Mark m, CanBeCursor m) => MarkedText m
cursorText = MarkedText "" [((0, 0), mkCursor)]

instance (Eq m, Mark m, CanBeCursor m)
      => Interactive ib (MarkedText m) where
  keyboard = simpleKeyboard textKeyHandler

instance (Eq m, Mark m, CanBeCursor m)
      => GtkDisplay (MarkedText m) where
  display = displayDiagram
          $ \mt -> scaleY (-1)
                 $ strutY 18
                   ===
                  ( strutX 10 ||| alignT (diagram mt) )

textKeyHandler :: (Eq m, Mark m, CanBeCursor m)
               => KeyEvent -> MarkedText m -> MarkedText m
textKeyHandler (True, ev) mt = case ev of
  Right k -> insert [k]
  Left  k -> case k of
    "Return"    -> insert "\n"
    "Left"      -> mutateCursors (subtract 1)
    "Right"     -> mutateCursors (+1)
    "Home"      -> mutateCursors (const (-maxIx, -maxIx))
    "End"       -> mutateCursors (const (maxIx, maxIx))
    "Delete"    -> editCursors (\(ivl, _) -> (second  (+1)  ivl, cursorText))
    "BackSpace" -> editCursors (\(ivl, _) -> (first (+(-1)) ivl, cursorText))
    "Escape"    -> unsafePerformIO $ (quitToy >> return mt)
    _           -> mt
 where
  editCursors f = edit (whenMarked isCursor f) mt

  insert s = editCursors $ second $ const (MarkedText s [((p, p), mkCursor)])
    where p = length s

  mutateCursors f = mutateMarks
                    ( \(i, m) -> if isCursor m then Just (f i, m) else Just (i, m) )
                    mt

  maxIx = textLength mt

textKeyHandler _ ts = ts


-- Utils
mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f = f *** f

firstA :: Applicative m => (a -> m b) -> (a, c) -> m (b, c)
firstA f  = raiseFstA . first f

raiseFstA :: Applicative m => (m a, t) -> m (a, t)
raiseFstA (x, y) = (,y) <$> x

{-
secondA :: Applicative m => (a -> m b) -> (c, a) -> m (c, b)
secondA f = raiseSndA . second f

raiseSndA :: Applicative m => (t, m a) -> m (t, a)
raiseSndA (x, y) = (x,) <$> y
-}

flipOrd :: Ordering -> Ordering
flipOrd LT = GT
flipOrd EQ = EQ
flipOrd GT = LT

-- Integer interval utilities

ivlIntersect :: Ivl -> Ivl -> Maybe Ivl
ivlIntersect (f1, t1) (f2, t2)
  | f2 >= t1 = Nothing
  | f1 >= t2 = Nothing
  | otherwise = Just (max f1 f2, min t1 t2)

{-
ivlIntersectInc :: Ivl -> Ivl -> Maybe Ivl
ivlIntersectInc (f1, t1) (f2, t2)
  | f2 > t1 = Nothing
  | f1 > t2 = Nothing
  | otherwise = Just (max f1 f2, min t1 t2)

ivlOverlaps :: Ivl -> Ivl -> Bool
ivlOverlaps a@(f1, t1) b@(f2, t2)
 =  ivlContains a f2
 || ivlContains a t2
 || ivlContains b f1
 || ivlContains b t1
-}

ivlContains :: Ivl -> Int -> Bool
ivlContains (f, t) x = f <= x && x <= t

ivlContainsIvl :: Ivl -> Ivl -> Bool
ivlContainsIvl i (f, t) = ivlContains i f && ivlContains i t

ivlUnion :: Ivl -> Ivl -> Ivl
ivlUnion (f1, t1) (f2, t2) = (min f1 f2, max t1 t2)

ivlMaybeUnion :: Ivl -> Ivl -> Maybe Ivl
ivlMaybeUnion a@(f1, t1) b@(f2, t2)
  | f2 > t1 = Nothing
  | f1 > t2 = Nothing
  | otherwise = Just $ ivlUnion a b

ivlSlices :: Ivl -> [Int]
ivlSlices (a, b) = [a, b]

{-
ivlOffset :: Int -> Ivl -> Ivl
ivlOffset x (a, b) = (a + x, b + x)
-}