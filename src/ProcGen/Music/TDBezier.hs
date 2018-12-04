-- | A data type for expressing time domain (TD) functions as Cubic Bezier splines curves. These
-- curves are essentially 2D images with time along the horizontal of the image and sample amplitude
-- along the vertical of the image.
--
-- However unlike an ordinary 2D cubic bezier spline, a time splines are linear along the time
-- dimension, rather than cubic, and furthermore the sample amplitude control points are restricted
-- to always be directly above or below the start and end points, in order to prevent curves that
-- can go both forward and backward in time.
module ProcGen.Music.TDBezier
  ( Ord3Spline, Ord3Part(..), Ord3Segment(..), StartPoint, EndPoint, ControlPoint,
    HasControlPoint1(..), HasControlPoint2(..), HasControlPoint3(..),
    ord3Window, ord3Duration, ord3Start, ord3End,
    ord3SegmentCount, ord3Segments, ord3Parts, ord3Spline,
  )
  where

import           ProcGen.Types

import           Control.Lens
import           Control.Monad

import           Data.Semigroup
import qualified Data.Vector.Unboxed              as Unboxed
import qualified Data.Vector.Unboxed.Mutable      as Mutable

----------------------------------------------------------------------------------------------------

class HasControlPoint1 seg where { ctrlPt1 :: Lens' seg ControlPoint; }
class HasControlPoint2 seg where { ctrlPt2 :: Lens' seg ControlPoint; }
class HasControlPoint3 seg where { ctrlPt3 :: Lens' seg ControlPoint; }

type StartPoint   = Sample
type ControlPoint = Sample
type EndPoint     = Sample

-- | A vector that defines a single segment of a continuous time domain bezier spline. You can
-- construct a 'Ord3Spline' spline by passing a list of these to the 'ord3Spline' function.
data Ord3Part
  = Ord3Part
    { ord3PartDt :: Duration
    , ord3PartP1 :: ControlPoint
    , ord3PartP2 :: ControlPoint
    , ord3PartP3 :: EndPoint
    }
  deriving (Eq, Show)

instance HasControlPoint1 Ord3Part where
  ctrlPt1 = lens ord3PartP1 $ \ a b -> a{ ord3PartP1 = b }

instance HasControlPoint2 Ord3Part where
  ctrlPt2 = lens ord3PartP2 $ \ a b -> a{ ord3PartP2 = b }

instance HasControlPoint3 Ord3Part where
  ctrlPt3 = lens ord3PartP3 $ \ a b -> a{ ord3PartP3 = b }

-- | This is a single segment of a 'TDBezier3Spline' expressed in absolute time. Unlike the
-- 'Ord3Part' (which is a part of a whole spline of connected segments), this data has it's own
-- 'TimeWindow' expressed in non-normalized, absolute units of time, making each segment a fully
-- independent polynomial. You cannot construct 'Ord3Bezier' splines from this data type, so there
-- are no lenses defined for this type. But this data type is more useful for computing the value of
-- the spline at a point in time.
--
-- This data type is also fully lazy, as it is intended to be used in intermediate computations and
-- then thrown away, rather than being stored in some other data structure.
data Ord3Segment
  = Ord3Segment
    { ord3Time :: (TimeWindow Moment)
    , ord3p0   :: StartPoint
    , ord3p1   :: ControlPoint
    , ord3p2   :: ControlPoint
    , ord3p3   :: EndPoint
    }

instance TimeDomain Ord3Segment ProcGenFloat where
  sample seg t = let win = ord3Time seg in
    bezier3 (ord3p0 seg) (ord3p1 seg) (ord3p2 seg) (ord3p3 seg) $
    (t - timeStart win) / twDuration win

instance HasTimeWindow Ord3Segment Moment where { timeWindow = Just . ord3Time; }

-- | A 'TimeDomain' spline constructed from the cubic Bezier formula. A 'TimeDomain' spline is
-- constructed of several segments, each segment has a time 'Duration'. For each consucutive time
-- interval specified by 'Duration' there is an associated 'StartPoint', 'EndPoint', and 2
-- 'ControlPoint's which determin the resultant 'Sample' value along that time interval. To compute
-- a 'Ord3Spline' as a function, use the 'sample' function of the 'TimeDomain' type class.
data Ord3Spline
  = Ord3Spline
    { ord3Window :: !(TimeWindow Moment)
    , ord3Start  :: !StartPoint
      -- ^ The first control point, which sets value of the spline at and before the 'timeStart' fo
      -- the 'ord3Window'.
    , ord3Points :: !(Unboxed.Vector ControlPoint)
    }
  deriving Eq

instance Ord Ord3Spline where
  compare a b =
    compare (ord3Window b) (ord3Window a) <>
    compare (ord3Start  a) (ord3Start  b) <>
    compare (ord3Points a) (ord3Points b)

--instance Semigroup Ord3Spline where
--  a <> b = ord3Append a subtract id id b

instance TimeDomain Ord3Spline ProcGenFloat where
  sample = ord3Sample
  iterateSamples spline nelems iter
    | nelems <= 0 = []
    | not iterFwd = 
      if splnFwd -- iterating in reverse over a forward spline
       then
        if timeEnd   iter >= timeEnd   swin then all ord3End else
        if timeStart iter <= timeStart swin then all ord3Start else
        let (hi, (mid, lo)) = span3 (>= (timeEnd   swin)) (>= (timeStart swin)) idx in
          zip hi (repeat $ ord3End   spline) ++ loop mid (reverse segs) ++
          zip lo (repeat $ ord3Start spline)
       else -- iterating in reverse over a reverse spline
        if timeStart iter <= timeEnd   swin then all ord3End   else
        if timeEnd   iter >= timeStart swin then all ord3Start else
        let (hi, (mid, lo)) = span3 (>= (timeStart swin)) (>= (timeEnd  swin)) idx in
          zip hi (repeat $ timeStart swin) ++ loop mid segs ++ zip lo (repeat $ timeEnd swin)
    | otherwise   =
      if splnFwd -- iterating forward over a forward spline
       then
        if timeEnd iter <= timeStart swin then all ord3Start else
        if timeStart iter >= timeEnd swin then all ord3End   else
        let (lo, (mid, hi)) = span3 (<= (timeStart swin)) (<= (timeEnd   swin)) idx in
          zip lo (repeat $ ord3Start spline) ++ loop mid segs ++ zip hi (repeat $ ord3End spline)
       else -- iterating forward over a reverse spline
        if timeStart iter >= timeStart swin then all ord3Start else
        if timeEnd   iter <= timeEnd   swin then all ord3End   else
        let (lo, (mid, hi)) = span3 (<= (timeEnd   swin)) (<= (timeStart swin)) idx in
          zip lo (repeat $ ord3End   spline) ++ loop mid (reverse segs) ++
          zip hi (repeat $ ord3Start spline)
    -- TODO: find a more elegant, mathematical way to do this without all these darn conditionals.
    where
      swin        = ord3Window spline
      iterFwd     = twDuration iter >= 0
      splnFwd     = twDuration swin >= 0
      stepsize    = twDuration iter / realToFrac nelems
      idx         = [timeStart iter + stepsize * realToFrac i | i <- [0 .. nelems]]
      segs        = ord3Segments spline
      span3 p1 p2 = fmap (span p2) . span p1
      all getVal  = idx `zip` repeat (getVal spline)
      loop :: [Moment] -> [Ord3Segment] -> [(Moment, Sample)]
      loop idx0   = \ case
        []       -> []
        seg:segs -> let (idx, more) = span (twContains $ ord3Time seg) idx0 in
          zip idx (sample seg <$> idx) ++ loop more segs

-- | Similar to 'ord3Start', gets the final control point that sets the value of the curve at and
-- beyond the 'timeEnd' of the 'ord3Window'.
ord3End :: Ord3Spline -> Sample
ord3End (Ord3Spline{ord3Start=p0,ord3Points=vec}) = let len = Unboxed.length vec in
  if len <= 0 then p0 else vec Unboxed.! (len - 1)

-- | Get the time span (the 'twDuration') of the 'ord3Window', that is the total amount of time this
-- spline spans.
ord3Duration :: Ord3Spline -> Duration
ord3Duration = twDuration . ord3Window

-- | This function is identical to the 'ProcGen.Types.sample' function, except it is not polymorphic
-- over the function type @f@, the type @f@ is 'Ord3Spline' when this function is used.
ord3Sample :: Ord3Spline -> Moment -> Sample
ord3Sample spline t = case filter ((`twContains` t) . ord3Time) $ ord3Segments spline of
  []    -> let TimeWindow{timeStart=lo} = twCanonicalize (ord3Window spline) in
    (if t <= lo then ord3Start else ord3End) spline
  seg:_ -> sample seg t
{-# INLINE ord3Sample #-}

-- | How many line segments exist in a 'Ord3Spline'.
ord3SegmentCount :: Ord3Spline -> Int
ord3SegmentCount = (`div` 4) . Unboxed.length . ord3Points

-- | Obtain the list of 'Ord3Part's used to construct this 'Ord3Spline'. Any 'Ord3Part' for which
-- the 'ord3PartDt' value is less than or equal to zero will have been deleted and so will not be
-- included in the list. The list will also have all 'ord3PartDt' values normalized.
ord3Parts :: Ord3Spline -> [Ord3Part]
ord3Parts spline = loop $ Unboxed.toList $ ord3Points spline where
  loop = \ case
    []               -> []
    dt:p1:p2:p3:more ->
      Ord3Part{ ord3PartDt = dt, ord3PartP1 = p1, ord3PartP2 = p2, ord3PartP3 = p3 } : loop more
    _ -> error $ let len = Unboxed.length $ ord3Points spline in
      "INTERNAL: Ord3Spline vector must contain a number of elements divisible by 4, "++
      "but vector contains "++show len++
      " elements, having "++show (mod len 4)++" extraneous elements."

-- | Obtain a list of line segments, each segment containing a start point, a control point near the
-- start point, a control point near the end point, and an end point.
ord3Segments :: Ord3Spline -> [Ord3Segment]
ord3Segments spline@(Ord3Spline{ord3Start=p0,ord3Window=win}) = loop 0 p0 $ ord3Parts spline where
  loop t0 p0 = \ case
    []        -> []
    part:more -> let t1 = t0 + ord3PartDt part in
      (Ord3Segment
       { ord3Time = TimeWindow
           { timeStart = timeStart win + t0 * twDuration win
           , timeEnd   = timeStart win + t1 * twDuration win
           }
       , ord3p0 = p0
       , ord3p1 = ord3PartP1 part
       , ord3p2 = ord3PartP2 part
       , ord3p3 = ord3PartP3 part
       }) : loop t1 (ord3PartP3 part) more

---- | Append two bezier splines. A new spline segment is created with the starting point as the
---- 'EndPoint' of the first spline, and the ending point as the 'StartPoint' of the second
---- spline. You must specify a time 'Duration' and 2 'ControlPoint's for the new segment as well,
---- although these parameters will be ignored if the 'EndPoint' of the first spline and the
---- 'StartPoint' of the second spline are exactly equal.
--ord3Append
--  :: Ord3Spline -- ^ the first 'Ord3Spline' spline
--  -> (EndPoint -> StartPoint -> Duration)
--      -- ^ Construct the time 'Duration' to be set between the first and second 'Ord3Spline' splines,
--      -- taking the 'EndPoint' of the first 'Ord3Spline' spline and the 'StartPoint' of the second
--      -- 'Ord3Spline' spline as inputs.
--  -> (EndPoint -> ControlPoint)
--      -- ^ Construct the first control point, taking the 'EndPoint' of the first 'Ord3Spline' spline.
--  -> (StartPoint -> ControlPoint)
--      -- ^ Construct the second control point, taking the 'StartPoint' of the second 'Ord3Spline'
--      -- spline.
--  -> Ord3Spline -- ^ the second 'Ord3Spline' spline
--  -> Ord3Spline
--ord3Append a mkDt mkCp1 mkCp2 b = Ord3Spline
--  { ord3Duration = ord3Duration a + ord3Duration b
--  , ord3Start    = ord3Start    a
--  , ord3Points   = Unboxed.create $ do
--      let vecA     = ord3Points a
--      let vecAlen  = Unboxed.length vecA
--      let vecB     = ord3Points b
--      let p0       = if Unboxed.null vecA then ord3Start a else vecA Unboxed.! (vecAlen - 1)
--      let p3       = ord3Start b
--      let seamless = p0 == p3
--      let pointsB  = Unboxed.toList vecB
--      let newLen   = (if seamless then id else (+ 4)) $ vecAlen + Unboxed.length vecB
--      vec <- Mutable.new newLen
--      mapM_ (uncurry $ Mutable.write vec) $ zip [0 .. newLen - 1] $ Unboxed.toList vecA ++
--        (if seamless then pointsB else mkDt p0 p3 : mkCp1 p0 : mkCp2 p3 : p3 : pointsB)
--      return vec
--  }

-- | Construct a 'Ord3Curve' from an initial point, followed by list of line segments. The list
-- takes the form of a 3-tuple with the near control point, the far control point, and the end point
-- of the sement. If you know how many line segments there are, pass that value as the first
-- parameter. A spline is constructed with a list of 'Ord3Part's, if the 'ord3PartDt' is less than
-- or equal to zero, it is dropped from the list and not used.
ord3Spline :: Maybe Int -> TimeWindow Moment -> StartPoint -> [Ord3Part] -> Ord3Spline
ord3Spline nparts win p0 parts' = spline where
  parts   = filter ((> 0) . ord3PartDt) parts'
  winsize = sum $ abs . ord3PartDt <$> parts
  spline  = Ord3Spline
    { ord3Window = win
    , ord3Start  = p0
    , ord3Points = maybe Unboxed.fromList makevec nparts $ loop parts 
    }
  makevec n elems = Unboxed.create $ do
    vec <- Mutable.new $ n * 4
    forM_ (zip [0 .. n*4] elems) $ \ (i, p) -> Mutable.write vec i p
    return vec
  loop = \ case
    []     -> []
    p:more -> (ord3PartDt p / winsize) : ord3PartP1 p : ord3PartP2 p : ord3PartP3 p : loop more
  -- The 'ord3PartDt' values are all normalized such that they add to 1.0, thus storing a ratio of
  -- the given 'twDuration' of the 'TimeWindow' value used to construct this spline.
