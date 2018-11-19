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
    ord3Duration, ord3Start, ord3Points,
    ord3SegmentCount, ord3Segments, ord3Spline, ord3Append,
  )
  where

import           ProcGen.Types

import           Control.Lens
import           Control.Monad

import           Data.Function                    (fix)
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
    { ord3PartDt :: !Duration
    , ord3PartP1 :: !ControlPoint
    , ord3PartP2 :: !ControlPoint
    , ord3PartP3 :: !EndPoint
    }
  deriving (Eq, Show)

instance HasControlPoint1 Ord3Part where
  ctrlPt1 = lens ord3PartP1 $ \ a b -> a{ ord3PartP1 = b }

instance HasControlPoint2 Ord3Part where
  ctrlPt2 = lens ord3PartP2 $ \ a b -> a{ ord3PartP2 = b }

instance HasControlPoint3 Ord3Part where
  ctrlPt3 = lens ord3PartP3 $ \ a b -> a{ ord3PartP3 = b }

-- | This is a segment of a 'TDBezier3Spline' expressed in absolute time. Unlike the
-- 'TDBezier3ContSeg' (which is a part of a whole spline of connected segments), this data does not
-- have a 'Duration' element but a start 'Moment' and end 'Moment', which expresses each segment as
-- a stand-alone segment, rather than as a part of a whole. You cannot construct 'TDBezeri3' splines
-- from this data type, so there are no lenses defined for this type. But this data type is more
-- useful for computing the value of the spline at a point in time.
data Ord3Segment
  = Ord3Segment
    { ord3Time :: !(TimeWindow Moment)
    , ord3p0   :: !StartPoint
    , ord3p1   :: !ControlPoint
    , ord3p2   :: !ControlPoint
    , ord3p3   :: !EndPoint
    }

instance TimeDomain Ord3Segment where
  sample seg = bezier3 (ord3p0 seg) (ord3p1 seg) (ord3p2 seg) (ord3p3 seg)

instance HasTimeWindow Ord3Segment Moment where { timeWindow = Just . ord3Time; }

-- | A 'TimeDomain' spline constructed from the cubic Bezier formula. A 'TimeDomain' spline is
-- constructed of several segments, each segment has a time 'Duration'. For each consucutive time
-- interval specified by 'Duration' there is an associated 'StartPoint', 'EndPoint', and 2
-- 'ControlPoint's which determin the resultant 'Sample' value along that time interval. To compute
-- a 'Ord3Spline' as a function, use the 'sample' function of the 'TimeDomain' type class.
data Ord3Spline
  = Ord3Spline
    { ord3Duration :: !Duration
    , ord3Start    :: !StartPoint
    , ord3Points   :: !(Unboxed.Vector ControlPoint)
    }
  deriving Eq

instance Ord Ord3Spline where
  compare a b =
    compare (ord3Duration b) (ord3Duration a) <>
    compare (ord3Start    a) (ord3Start    b) <>
    compare (ord3Points   a) (ord3Points   b)

instance Semigroup Ord3Spline where
  a <> b = ord3Append a subtract id id b

instance TimeDomain Ord3Spline where
  sample = ord3Sample
  iterateSamples spline nelems lo hi = if nonsense then [] else loop idx trimsegs where
    top   = ord3Duration spline
    nonsense = nelems <= 0 || lo < 0 && hi < 0 || lo > top && hi > top
    small = min lo hi
    stepsize = (hi - lo) / realToFrac nelems
    base  = if small >= 0 then small else
      (realToFrac (ceiling (abs $ small / stepsize) :: Int)) * abs stepsize + small
    count = floor ((top - base) / abs stepsize) :: Int
    roof  = base + abs stepsize * realToFrac count
    forward  = lo <= hi
    segs  = (if forward then id else reverse) $ ord3Segments spline
    idx   = [(if forward then base else roof) + stepsize * realToFrac i | i <- [0 .. count]]
    trimsegs = case idx of
      []  -> []
      i:_ -> dropWhile (not . (`twContains` i) . ord3Time) segs
    loop idx segs = case segs of
      []       -> []
      seg:segs -> let (here, next) = span (twContains $ ord3Time seg) idx in
        sample seg <$> here ++ loop next segs

-- | This function is identical to the 'ProcGen.Types.sample' function, except it its not
-- polymorphic over the function type @f@, the type @f@ is 'Ord3Spline' when this function is used.
ord3Sample :: Ord3Spline -> Moment -> Sample
ord3Sample spline t = case filter ((`twContains` t) . ord3Time) $ ord3Segments spline of
  []    -> a0
  seg:_ -> if 0 > t then a0 else
    if t >= ord3Duration spline
     then if len == 0 then a0 else vec Unboxed.! len - 1
     else sample seg $ (t - (timeStart $ ord3Time seg)) / twDuration (ord3Time seg)
  where
    a0  = ord3Start spline
    vec = ord3Points spline
    len = Unboxed.length vec
{-# INLINE ord3Sample #-}

-- | How many line segments exist in a 'Ord3Spline'.
ord3SegmentCount :: Ord3Spline -> Int
ord3SegmentCount = (`div` 4) . Unboxed.length . ord3Points

-- | Obtain a list of line segments, each segment containing a start point, a control point near the
-- start point, a control point near the end point, and an end point.
ord3Segments :: Ord3Spline -> [Ord3Segment]
ord3Segments (Ord3Spline{ord3Start=p0,ord3Points=vec}) = fix
  ( \ loop t0 p0 -> \ case
    []               -> []
    dt:p1:p2:p3:more -> let t1 = t0 + dt in
      (Ord3Segment
       { ord3Time = TimeWindow{ timeStart = t0, timeEnd = t1 }
       , ord3p0 = p0, ord3p1 = p1, ord3p2 = p2, ord3p3 = p3
       }) : loop t1 p3 more
    _      -> error $ let len = Unboxed.length vec in
      "INTERNAL: Ord3Spline vector must contain a number of elements divisible by 4, "++
      "but vector contains "++show len++
      " elements, having "++show (mod len 4)++" extraneous elements."
  ) 0 p0 (Unboxed.toList vec)

-- | Append two bezier splines. A new spline segment is created with the starting point as the
-- 'EndPoint' of the first spline, and the ending point as the 'StartPoint' of the second
-- spline. You must specify a time 'Duration' and 2 'ControlPoint's for the new segment as well,
-- although these parameters will be ignored if the 'EndPoint' of the first spline and the
-- 'StartPoint' of the second spline are exactly equal.
ord3Append
  :: Ord3Spline -- ^ the first 'Ord3Spline' spline
  -> (EndPoint -> StartPoint -> Duration)
      -- ^ Construct the time 'Duration' to be set between the first and second 'Ord3Spline' splines,
      -- taking the 'EndPoint' of the first 'Ord3Spline' spline and the 'StartPoint' of the second
      -- 'Ord3Spline' spline as inputs.
  -> (EndPoint -> ControlPoint)
      -- ^ Construct the first control point, taking the 'EndPoint' of the first 'Ord3Spline' spline.
  -> (StartPoint -> ControlPoint)
      -- ^ Construct the second control point, taking the 'StartPoint' of the second 'Ord3Spline'
      -- spline.
  -> Ord3Spline -- ^ the second 'Ord3Spline' spline
  -> Ord3Spline
ord3Append a mkDt mkCp1 mkCp2 b = Ord3Spline
  { ord3Duration = ord3Duration a + ord3Duration b
  , ord3Start    = ord3Start    a
  , ord3Points   = Unboxed.create $ do
      let vecA     = ord3Points a
      let vecAlen  = Unboxed.length vecA
      let vecB     = ord3Points b
      let p0       = if Unboxed.null vecA then ord3Start a else vecA Unboxed.! (vecAlen - 1)
      let p3       = ord3Start b
      let seamless = p0 == p3
      let pointsB  = Unboxed.toList vecB
      let newLen   = (if seamless then id else (+ 4)) $ vecAlen + Unboxed.length vecB
      vec <- Mutable.new newLen
      mapM_ (uncurry $ Mutable.write vec) $ zip [0 .. newLen - 1] $ Unboxed.toList vecA ++
        (if seamless then pointsB else mkDt p0 p3 : mkCp1 p0 : mkCp2 p3 : p3 : pointsB)
      return vec
  }

-- | Construct a 'Ord3Curve' from an initial point, followed by list of line segments. The list
-- takes the form of a 3-tuple with the near control point, the far control point, and the end point
-- of the sement. If you know how many line segments there are, pass that value as the first
-- parameter.
ord3Spline :: Maybe Int -> StartPoint -> [Ord3Part] -> Ord3Spline
ord3Spline nparts p0 parts = spline where
  spline = Ord3Spline
    { ord3Duration = sum $ abs . ord3PartDt <$> parts
    , ord3Start    = p0
    , ord3Points   = maybe Unboxed.fromList makevec nparts $ loop parts 
    }
  makevec n elems = Unboxed.create $ do
    vec <- Mutable.new $ n * 4
    forM_ (zip [0 .. n*4] elems) $ \ (i, p) -> Mutable.write vec i p
    return vec
  loop = \ case
    []     -> []
    p:more -> ord3PartDt p : ord3PartP1 p : ord3PartP2 p : ord3PartP3 p : loop more
