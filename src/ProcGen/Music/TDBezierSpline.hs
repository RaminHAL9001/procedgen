module ProcGen.Music.TDBezierSpline
  ( TDBezier3, bezier3Duration, bezier3Start, bezier3Points,
    bezier3SegmentCount, bezier3Segments, bezier3Spline, bezier3Append,
  )
  where

import           ProcGen.Types

import           Control.Monad

import           Data.Semigroup
import qualified Data.Vector.Unboxed              as Unboxed
import qualified Data.Vector.Unboxed.Mutable      as Mutable

----------------------------------------------------------------------------------------------------

type StartPoint   = Sample
type ControlPoint = Sample
type EndPoint     = Sample

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
    stepsize = (hi - lo) / realToFrac nelems
    small = min lo hi
    large = max lo hi
    base  = if small >= 0 then small else
      (realToFrac (ceiling (abs $ small / stepsize) :: Int)) * abs stepsize + small
    count = floor ((top - base) / abs stepsize) :: Int
    roof  = base + abs stepsize * realToFrac count
    segs  = (if stepsize > 0 then id else reverse) $ bezier3Segments spline
    idx   = [(if stepsize < 0 then roof else base) + stepsize * realToFrac i | i <- [0 .. count]]
    trimsegs = case idx of
      []  -> []
      i:_ -> error $
        "TODO: dropWhile segments are below the initial index, perhaps rewrite "++
        "the 'bezier3Sample' function to output the start and end times for each"++
        "segment, rather than just the time delta. The 'bezier3Sample' function "++
        "could then be rewritten to take advantage of this."

bezier3Sample :: TDBezier3 -> Moment -> Sample
bezier3Sample spline t = case bezier3Segments spline of
  []      -> 0
  px:more ->
    if 0 > t || t >= bezier3Duration spline then 0 else loop 0 px more where
      loop t0 (dt', p0, p1, p2, p3) more = let dt = t0 + dt' in
        if t0 <= t && t < dt then bezier3 p0 p1 p2 p3 ((t - t0) / dt') else case more of
          []      -> 0
          px:more -> loop dt px more
{-# INLINE bezier3Sample #-}

-- | How many line segments exist in a 'TDBezier3'.
bezier3SegmentCount :: TDBezier3 -> Int
bezier3SegmentCount = (`div` 4) . Unboxed.length . bezier3Points

-- | Obtain a list of line segments, each segment containing a start point, a control point near the
-- start point, a control point near the end point, and an end point.
bezier3Segments :: TDBezier3 -> [(Duration, StartPoint, ControlPoint, ControlPoint, EndPoint)]
bezier3Segments (TDBezier3{bezier3Start=p0,bezier3Points=vec}) = loop p0 $ Unboxed.toList vec where
  loop p0 = \ case
    []               -> []
    dt:p1:p2:p3:more -> (dt, p0, p1, p2, p3) : loop p3 more
    _          -> error $
      "INTERNAL: TDBezier3 vector must contain a number of elements divisible by 4, "++
      "but vector contains "++show (Unboxed.length vec)++" elements."

bezier3IndexSafe
  :: TDBezier3 -> Int
  -> Maybe (Duration, StartPoint, ControlPoint, ControlPoint, EndPoint)
bezier3IndexSafe = error "TODO: bezier3IndexSafe"

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
  -> [(Duration, ControlPoint, ControlPoint, EndPoint)]
  -> TDBezier3
bezier3Spline nsegs p0 segs = spline where
  spline = TDBezier3
    { bezier3Duration = sum $ abs . fstOf4 <$> segs
    , bezier3Start    = p0
    , bezier3Points   = maybe Unboxed.fromList makevec nsegs $ loop segs 
    }
  fstOf4 (dt, _, _, _) = dt
  makevec n elems = Unboxed.create $ do
    vec <- Mutable.new $ n * 4
    forM_ (zip [0 .. n*4] elems) $ \ (i, p) -> Mutable.write vec i p
    return vec
  loop = \ case
    []                    -> []
    (dt, p1, p2, p3):more -> dt : p1 : p2 : p3 : loop more
