-- | A data type for expressing time domain (TD) functions as Cubic Bezier splines curves. These
-- curves are essentially 2D images with time along the horizontal of the image and sample amplitude
-- along the vertical of the image.
--
-- However unlike an ordinary 2D cubic bezier spline, a time splines are linear along the time
-- dimension, rather than cubic, and furthermore the sample amplitude control points are restricted
-- to always be directly above or below the start and end points, in order to prevent curves that
-- can go both forward and backward in time.
module ProcGen.Music.TDBezierSpline
  ( TDBezier3, TDBezier3Part(..), TDBezier3Segment(..),
    bezier3Duration, bezier3Start, bezier3Points,
    bezier3SegmentCount, bezier3Segments, bezier3Spline, bezier3Append,
  )
  where

import           ProcGen.Types

import           Control.Monad

import           Data.Function                    (fix)
import           Data.Semigroup
import qualified Data.Vector.Unboxed              as Unboxed
import qualified Data.Vector.Unboxed.Mutable      as Mutable

----------------------------------------------------------------------------------------------------

type StartPoint   = Sample
type ControlPoint = Sample
type EndPoint     = Sample

-- | A vector that defines a single segment of a continuous time domain bezier spline. You can
-- construct a 'TDBezier3' spline by passing a list of these to the 'bezier3Spline' function.
data TDBezier3Part
  = TDBezier3Part
    { tdBezier3PartDuration :: !Duration
    , tdBezier3PartCtrlP1   :: !ControlPoint
    , tdBezier3PartCtrlP2   :: !ControlPoint
    , tdBezier3PartEndPoint :: !EndPoint
    }
  deriving (Eq, Show)

-- | This is a segment of a 'TDBezier3Spline' expressed in absolute time. Unlike the
-- 'TDBezier3ContSeg' (which is a part of a whole spline of connected segments), this data does not
-- have a 'Duration' element but a start 'Moment' and end 'Moment', which expresses each segment as
-- a stand-alone segment, rather than as a part of a whole. You cannot construct 'TDBezeri3' splines
-- from this data type, but this time is more useful for computing the value of the spline at a
-- point in time.
data TDBezier3Segment
  = TDBezier3Segment
    { tdBezier3StartTime  :: !Moment
    , tdBezier3EndTime    :: !Moment
    , tdBezier3StartPoint :: !StartPoint
    , tdBezier3CtrlP1     :: !ControlPoint
    , tdBezier3CtrlP2     :: !ControlPoint
    , tdBezier3EndPoint   :: !EndPoint
    }

-- | Returns true if the given 'Moment' is within the 'tdBezier3StartTime' and the
-- 'tdBezier3EndTime'.
tdBezier3SegmentAround :: TDBezier3Segment -> Moment -> Bool
tdBezier3SegmentAround seg t = tdBezier3StartTime seg <= t && t < tdBezier3EndTime seg

instance TimeDomain TDBezier3Segment where
  sample seg = bezier3
    (tdBezier3StartPoint seg)
    (tdBezier3CtrlP1     seg)
    (tdBezier3CtrlP2     seg)
    (tdBezier3EndPoint   seg)

-- | A 'TimeDomain' spline constructed from the cubic Bezier formula. A 'TimeDomain' spline is
-- constructed of several segments, each segment has a time 'Duration'. For each consucutive time
-- interval specified by 'Duration' there is an associated 'StartPoint', 'EndPoint', and 2
-- 'ControlPoint's which determin the resultant 'Sample' value along that time interval. To compute
-- a 'TDBezier3' as a function, use the 'sample' function of the 'TimeDomain' type class.
data TDBezier3
  = TDBezier3
    { bezier3Duration :: !Duration
    , bezier3Start    :: !StartPoint
    , bezier3Points   :: !(Unboxed.Vector ControlPoint)
    }
  deriving Eq

instance Ord TDBezier3 where
  compare a b =
    compare (bezier3Duration b) (bezier3Duration a) <>
    compare (bezier3Start    a) (bezier3Start    b) <>
    compare (bezier3Points   a) (bezier3Points   b)

instance Semigroup TDBezier3 where
  a <> b = bezier3Append a subtract id id b

instance TimeDomain TDBezier3 where
  sample = bezier3Sample
  iterateSamples spline nelems lo hi = if nonsense then [] else loop idx trimsegs where
    top   = bezier3Duration spline
    nonsense = nelems <= 0 || lo < 0 && hi < 0 || lo > top && hi > top
    small = min lo hi
    stepsize = (hi - lo) / realToFrac nelems
    base  = if small >= 0 then small else
      (realToFrac (ceiling (abs $ small / stepsize) :: Int)) * abs stepsize + small
    count = floor ((top - base) / abs stepsize) :: Int
    roof  = base + abs stepsize * realToFrac count
    forward  = lo <= hi
    segs  = (if forward then id else reverse) $ bezier3Segments spline
    idx   = [(if forward then base else roof) + stepsize * realToFrac i | i <- [0 .. count]]
    trimsegs = case idx of
      []  -> []
      i:_ -> dropWhile (not . (`tdBezier3SegmentAround` i)) segs
    loop idx segs = case segs of
      []       -> []
      seg:segs -> let (here, next) = span (tdBezier3SegmentAround seg) idx in
        sample seg <$> here ++ loop next segs

bezier3Sample :: TDBezier3 -> Moment -> Sample
bezier3Sample spline t = case filter (`tdBezier3SegmentAround` t) $ bezier3Segments spline of
  []    -> 0
  seg:_ -> if 0 > t || t >= bezier3Duration spline then 0 else sample seg $
    (t - tdBezier3StartTime seg) / (tdBezier3EndTime seg - tdBezier3StartTime seg)
{-# INLINE bezier3Sample #-}

-- | How many line segments exist in a 'TDBezier3'.
bezier3SegmentCount :: TDBezier3 -> Int
bezier3SegmentCount = (`div` 4) . Unboxed.length . bezier3Points

-- | Obtain a list of line segments, each segment containing a start point, a control point near the
-- start point, a control point near the end point, and an end point.
bezier3Segments :: TDBezier3 -> [TDBezier3Segment]
bezier3Segments (TDBezier3{bezier3Start=p0,bezier3Points=vec}) = fix
  ( \ loop t0 p0 -> \ case
    []               -> []
    dt:p1:p2:p3:more -> let t1 = t0 + dt in
      (TDBezier3Segment
       { tdBezier3StartTime  = t0
       , tdBezier3EndTime    = t1
       , tdBezier3StartPoint = p0
       , tdBezier3CtrlP1     = p1
       , tdBezier3CtrlP2     = p2
       , tdBezier3EndPoint   = p3
       }) : loop t1 p3 more
    _      -> error $ let len = Unboxed.length vec in
      "INTERNAL: TDBezier3 vector must contain a number of elements divisible by 4, "++
      "but vector contains "++show len++
      " elements, having "++show (mod len 4)++" extraneous elements."
  ) 0 p0 (Unboxed.toList vec)

-- | Append two bezier splines. A new spline segment is created with the starting point as the
-- 'EndPoint' of the first spline, and the ending point as the 'StartPoint' of the second
-- spline. You must specify a time 'Duration' and 2 'ControlPoint's for the new segment as well,
-- although these parameters will be ignored if the 'EndPoint' of the first spline and the
-- 'StartPoint' of the second spline are exactly equal.
bezier3Append
  :: TDBezier3 -- ^ the first 'TDBezier3' spline
  -> (EndPoint -> StartPoint -> Duration)
      -- ^ Construct the time 'Duration' to be set between the first and second 'TDBezier3' splines,
      -- taking the 'EndPoint' of the first 'TDBezier3' spline and the 'StartPoint' of the second
      -- 'TDBezier3' spline as inputs.
  -> (EndPoint -> ControlPoint)
      -- ^ Construct the first control point, taking the 'EndPoint' of the first 'TDBezier3' spline.
  -> (StartPoint -> ControlPoint)
      -- ^ Construct the second control point, taking the 'StartPoint' of the second 'TDBezier3'
      -- spline.
  -> TDBezier3 -- ^ the second 'TDBezier3' spline
  -> TDBezier3
bezier3Append a mkDt mkCp1 mkCp2 b = TDBezier3
  { bezier3Duration = bezier3Duration a + bezier3Duration b
  , bezier3Start    = bezier3Start    a
  , bezier3Points   = Unboxed.create $ do
      let vecA     = bezier3Points a
      let vecAlen  = Unboxed.length vecA
      let vecB     = bezier3Points b
      let p0       = if Unboxed.null vecA then bezier3Start a else vecA Unboxed.! (vecAlen - 1)
      let p3       = bezier3Start b
      let seamless = p0 == p3
      let pointsB  = Unboxed.toList vecB
      let newLen   = (if seamless then id else (+ 4)) $ vecAlen + Unboxed.length vecB
      vec <- Mutable.new newLen
      mapM_ (uncurry $ Mutable.write vec) $ zip [0 .. newLen - 1] $ Unboxed.toList vecA ++
        (if seamless then pointsB else mkDt p0 p3 : mkCp1 p0 : mkCp2 p3 : p3 : pointsB)
      return vec
  }

-- | Construct a 'Bezier3Curve' from an initial point, followed by list of line segments. The list
-- takes the form of a 3-tuple with the near control point, the far control point, and the end point
-- of the sement. If you know how many line segments there are, pass that value as the first
-- parameter.
bezier3Spline
  :: Maybe Int
  -> StartPoint
  -> [TDBezier3Part]
  -> TDBezier3
bezier3Spline nparts p0 parts = spline where
  spline = TDBezier3
    { bezier3Duration = sum $ abs . tdBezier3PartDuration <$> parts
    , bezier3Start    = p0
    , bezier3Points   = maybe Unboxed.fromList makevec nparts $ loop parts 
    }
  makevec n elems = Unboxed.create $ do
    vec <- Mutable.new $ n * 4
    forM_ (zip [0 .. n*4] elems) $ \ (i, p) -> Mutable.write vec i p
    return vec
  loop = \ case
    []     -> []
    p:more ->
        tdBezier3PartDuration p
      : tdBezier3PartCtrlP1   p
      : tdBezier3PartCtrlP2   p
      : tdBezier3PartEndPoint p
      : loop more
