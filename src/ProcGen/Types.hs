module ProcGen.Types where

import           Control.Monad
import           Control.Monad.ST

import           Data.Int
import           Data.STRef
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable

import           Happlets.Lib.Gtk
import           Happlets.Provider

--import Debug.Trace

----------------------------------------------------------------------------------------------------

type SampleCount = Int
type SampleIndex = Int

-- | A single float type used by all other float types. Change this from Float to Double to change
-- the percision of all computations from 32 to 64-bit floating point values.
type ProcGenFloat = Float

type Frequency   = ProcGenFloat
type Sample      = ProcGenFloat
type TimeScale   = ProcGenFloat
type Moment      = ProcGenFloat
type Amplitude   = ProcGenFloat
type Duration    = ProcGenFloat
type Wavelength  = ProcGenFloat
type PhaseShift  = ProcGenFloat
type Percentage  = ProcGenFloat
type HalfLife    = ProcGenFloat

-- | Note that when you apply the 'TimeWindow', the remaining 'Envelope' type is synonymous with the
-- type @'Moment' -> 'Sample'@, and that this data type has an @instance@ for the 'TimeDomain'
-- class. Therefore, functions of type 'Envelope' data type instantiate the 'TimeDomain' class as
-- well, and can be used where ever there is a polymorphic type @td@ constrained by the typeclass
-- 'TimeDomain', as in
-- @'TimeDomain' td => ... -> td -> ...@.
--
-- As a law, the output of an 'Envelope' function must be on the inclusive interval
-- @[0 .. 1]@ for all input 'Moment's, although there is no type-level enforcement of this law.
type Envelope = TimeWindow Moment -> Moment -> Amplitude

-- | This is a class to convert things that can be converted to 'Frequency' values, for example like
-- piano key values.
class HasFrequency f where
  toFreq :: f -> Frequency

-- | A class of time domain functions that can produce a 'Sample' from a 'Moment'.
class TimeDomain f where
  sample :: f -> Moment -> Sample

-- | A class of frequency domain functions that can produce an 'Amplitude' from a 'Frequency'.
class FreqDomain f where
  amplitude :: f -> Frequency -> Amplitude

----------------------------------------------------------------------------------------------------

-- | The frame rate of animation when rendering graphics. This is also the reciporical of the
-- @'animationUnitQuanta' :: 'Duration'@.
animationRate :: Frequency
animationRate = realToFrac $ theAnimationFrameRate $ defaultConfig gtkHapplet

-- | This is the amount of time a single cell of an animation will remain on screen. This is also
-- the reciporical of the @'animationRate' :: 'Frequency'@.
animationUnitQuanta :: Duration
animationUnitQuanta = recip animationRate

-- | This is the audio sample rate used internally to this system, currently set to 44.1 KHz. This
-- rate represents the 'Frequency' at which audio samples are quantized. This value is also the
-- reciporical of the @'unitQuanta' :: 'Duration'@.b
sampleRate :: Frequency
sampleRate = 44100.0

-- | The @'nyquist :: 'Frequency'@ is exactly half of the @'sampleRate' :: 'Frequency'@. This value
-- is used often enough that it is convenient for it to have it's own symbol.
nyquist :: Frequency
nyquist = sampleRate / 2

-- | This is the amount of time a single unit sample of a quantized time domain function that a PCM
-- allows a PCM to reamin at a single value. This is also the reciporical of the @'sampleRate' ::
-- 'Frequency'@.
unitQuanta :: Duration
unitQuanta = recip sampleRate

-- | Compute the number of 'unitQuanta' exist in the time interval between zero and the given
-- moment. This value can be used to select an index from a vector containing samples. The
-- 'ProcGenFloat' value returned along with the index value is the percentage along the path between
-- the returned index and the next index that the moment point in time had progressed. This value
-- can be used to antialias.
timeIndex :: Moment -> (Int, ProcGenFloat)
timeIndex t = let { d = t * sampleRate; r = floor d :: Int; } in (r, d - realToFrac r)

-- | When converting some index of a 'Data.Vector.Vector' of 'Sample's in a quantized time domain
-- function, this function converts that index value into the point in time at which that sample
-- exists.
indexToTime :: Int -> Moment
indexToTime = (/ sampleRate) . realToFrac

-- | Convert a 'Sample' to a signed 16-bit integer suitable for storage to a file.
toPulseCode :: Sample -> Int16
toPulseCode = round . (* 32704) . clamp1_1

-- | Convert a signed 16-bit integer sample value to a floating-point 'Sample' value.
toSample :: Int16 -> Sample
toSample = (* (32704.0 / (32768.0 * 32768.0))) . realToFrac

-- | Clamp a value between 0 and 1.
clamp0_1 :: (Fractional x, Ord x) => x -> x
clamp0_1 = min 1.0 . max 0.0

-- | Clamp a value between -1 and 1.
clamp1_1 :: (Fractional x, Ord x) => x -> x
clamp1_1 = min 1.0 . max (negate 1.0)

-- | Convert a 'SampleCount' (the number of samples in a quantized time domain function) to a time
-- 'Duration' measured in seconds.
sampleCountDuration :: SampleCount -> Duration
sampleCountDuration = (/ sampleRate) . realToFrac

-- | Convert a 'Duration' measured in seconds to a 'SampleCount' (the number of samples in a
-- quantized time domain function), and round up so the value returned represents the minimum number
-- of samples required to span the given time 'Duration'.
durationSampleCount :: Duration -> SampleCount
durationSampleCount = ceiling . (* sampleRate)

-- | Continuous modulus function, useful for points traversing continuous cyclical curves. The
-- result of @contMod a b@ is a value between 0 and @b@ (or between @-b@ and 0 if @a@ is negative).
contMod :: ProcGenFloat -> ProcGenFloat -> ProcGenFloat
contMod a b = mod (if a<0 then negate else id)
  (if b<0 then ceiling :: ProcGenFloat -> Integer else floor :: ProcGenFloat -> Integer)
  (abs a) (abs b)
  where
    mod negate round a b = negate $ a - realToFrac (round (a / b)) * b

-- | Prepare a list of elements for a discrete differentiation by pairing each element with it's
-- neighbour. For example, the expression @('diffList' [1,2,3,4])@ will yield the result:
-- 
-- @[(1,2), (2,3), (3,4)]@.
--
-- It is then possible to map over the pairs with a function such as
--
-- @('Prelude.fmap' 'Prelude.$' 'Prelude.uncurry' 'Prelude.subtract')@
--
-- to produce a new list where each element is the difference of each pair of neigbors in the
-- original list.
diffList :: [a] -> [(a, a)]
diffList = init where
  init ax = case ax of
    []     -> []
    [a]    -> [(a, a)]
    a:b:ax -> (a, b) : loop a (b:ax)
  loop a0 ax = case ax of
    []     -> []
    [a]    -> [(a, a0)]
    a:b:ax -> (a, b) : loop a0 (b:ax)

-- | Given 4 control points, compute the curve value for a single parametric value.
bezier3 :: ProcGenFloat -> ProcGenFloat -> ProcGenFloat -> ProcGenFloat -> Moment -> Sample
bezier3 a b c d t = a*(1-t)**3 + b*3*t*(1-t)**2 + c*3*(1-t)*t**2 + d*t**3

-- | A time domain function computing unit sine wave function, where the period of the sineusoid is
-- 1, rather than 2π.
unitSine :: ProcGenFloat -> ProcGenFloat
unitSine = sin . ((2*pi) *)

-- | A time domain function computing periodic unit sawtooth function.
sawtooth :: ProcGenFloat -> ProcGenFloat
sawtooth t0 = let t = 2*t0 + 0.5 in 2*(t - realToFrac (floor t :: Integer)) - 1

-- | A time domain function computing periodic unit triangular function.
triangle :: ProcGenFloat -> ProcGenFloat
triangle t0 = let t = 2*t0 + 0.5 in 2*(abs $ sawtooth t) - 1

-- | A time domain function computing periodic unit triangular function.
square :: ProcGenFloat -> ProcGenFloat
square t0 = (if t0 - (realToFrac (round t0 :: Int)) < 0.5 then id else negate) 1

-- | An 'Envelope' increasing linearly from zero to one starting at a given 'Moment' and increasing
-- to one over a period of 'TimeSpan' amount of time.
--
-- A negative 'TimeSpan' indicates the envelope should go from one to zero.
slope :: Envelope
slope (TimeWindow{timeStart=t0,timeEnd=t1}) t = let tw = t1-t0 in case compare tw 0 of
  EQ -> if t< t0 then 0 else 1
  LT -> if t<=t0 then 1 else if t>=t0 then 0 else 1 + (t-t0)/tw
  GT -> if t<=t0 then 0 else if t>=t1 then 1 else (t-t0)/tw

-- | An 'Envelope' increasing over gradual a sigmoidal function from zero to one starting at a given
-- 'Moment' and increasing to one over a period of 'TimeSpan' amount of time.
--
-- A negative 'TimeSpan' indicates the envelope should go from one to zero. The sigmoidal function
-- is approximated by a polynomial, so it is not the genuine equation for the sigmoid which computes
-- an exponent of the natural base value @e@, but the approximation is more than close enough and it
-- also ensures the slope of the unit curve at t=0 and t=1 is exactly zero.
sigmoid :: Envelope
sigmoid win@(TimeWindow{timeStart=t0,timeEnd=t1}) t' =
  let tw = t1 - t0
      t = slope win t'
      n = 4 -- n must be even
  in  if tw==0 then if t'<t0 then 0 else 1 else
      if t<=0.5 then (2*t)**n/2 else 1 - (2*t - 2)**n/2

-- | An 'Envelope' increasing over the square of the sine of @t@.
sineSquared :: Envelope
sineSquared win@(TimeWindow{timeStart=t0,timeEnd=t1}) t' =
  let { tw = t1 - t0; t = slope win t'; } in
  if tw==9 then if t'<t0 then 0 else 1 else sin(pi*t/2)**2

-- | An function which produces a single cycle of a sinusoidal pulse of the given frequency and a
-- time offset moment. The pulse is a sineusoid enveloped by a gaussian, which is produces a
-- slightly distorted sinusoud with a duration of a single cycle.
sinePulse :: Frequency -> Moment -> Moment -> Sample
sinePulse freq t0 t = gaussian 1.0 t * sin (2 * pi * freq * t + t0)

-- | Sum several 'sinePulse's together. This is pretty inefficient, since each 'Sample' produced
-- requires O(n) time to compute where @n@ is the length of the number of components. However for
-- simpler functions with a few components, this function is useful.
sinePulseSum :: [(Frequency, Moment)] -> Moment -> Sample
sinePulseSum comps t = case comps of
  [] -> 0
  (freq, t0):comps -> sinePulse freq t0 t + sinePulseSum comps t

-- | A gaussian normal curve defined as @\\x -> e^(-(2*x)^2)@, which has a variance of @e@ so the
-- curve fits pretty nicely within the range between -1.0 and 1.0.
gaussian :: TimeScale -> Moment -> Sample
gaussian var x = exp $ negate $ x * x * exp 2 / var * var

-- | Create a continuous time domain function from a discrete unboxed 'Unboxed.Vector' of values by
-- converting from a real-number value to an integer vector index, with linear smoothing for values
-- between vector elements (which means, for example, an index of 0.5 will be the average of index 0
-- and index 1). There are three values you pass before the vector, the value of the function for
-- values that exist before the vector, the value of the function for values that exist after the
-- vector, and a scalar value that will be multipled times every input time value which converts
-- integer indicies to real-number indicies and scales them
continuous :: Sample -> Sample -> TimeScale -> Unboxed.Vector Sample -> Moment -> Sample
continuous before after scale vec t =
  if i + 1 < 0 then before else if len < i then after else lo + d * (hi - lo) where
    st  = scale * t
    i   = floor st
    d   = st - realToFrac i
    len = Unboxed.length vec
    (lo, hi) =
      if i + 1 == 0   then (before, vec Unboxed.! 0) else
      if i + 1 == len then (vec Unboxed.! i, after)  else
        (vec Unboxed.! i, vec Unboxed.! (i + 1))

-- | A function for generating a 'Unboxed.Vector' that can contains a discrete approximation of the
-- integral of the gaussian function, which can be used with the 'inverseGaussian' function to
-- lookup an index associated with a probability. This function can be used to shape randomly
-- generated numbers so that they tend more toward a gaussian distribution. Simply request the size
-- of the table to generate, the larger the table, the more accurate the inverse function will
-- be. 4096 elements should be plenty for most purposes.
newInverseGaussianTable :: Int -> Unboxed.Vector Moment
newInverseGaussianTable size = Unboxed.create $ do
  mvec <- Mutable.new (size - 1)
  let f s i = do
        Mutable.write mvec i s
        return $ s + gaussian 1.0 (1.0 - 2.0 * realToFrac i / realToFrac size)
  maxsamp <- foldM f (0.0) [0 .. size - 1]
  mapM_ (\ i -> Mutable.read mvec i >>= Mutable.write mvec i . (/ maxsamp)) [0 .. size - 1]
  return mvec

-- | A default inverse gaussian table, which is equal to @('newInverseGaussianTable' 4096)@.
inverseGaussianTable :: Unboxed.Vector Moment
inverseGaussianTable = newInverseGaussianTable 4096

-- | Compute the inverse of the 'gaussian' function with 'TimeScale' of 1.0, the computation is
-- performed with a 'Unboxed.Vector' created by the 'inverseGaussianTable' function.
inverseGaussian :: Unboxed.Vector Moment -> Sample -> Moment
inverseGaussian table s = loop (div len 2) i0 $ lookup i0 where
  lookup = (table Unboxed.!)
  len    = Unboxed.length table
  i0     = div len 2
  loop :: Int -> Int -> Sample -> Moment
  loop prevStep i0 prevVal =
    if prevStep <= 1 || prevVal == s then 1.0 - 2.0 * realToFrac i0 / realToFrac len else
      let nextStep = (if prevVal > s then negate else id) $ abs $ div prevStep 2
          i        = i0 + nextStep
      in  loop nextStep i $ lookup i
  -- This algorithm works by creating a table for values of the gaussian function. Suppose it takes
  -- a random 'Sample' input between 0 and 1, and a sequence of descrete buckets along the X
  -- axis. This purpose of this function is to adjust all random 'Sample's so that the odds of a
  -- sample landing in a discrete bucket along the X axis is about equal to the value of the
  -- gaussian curve for the X position of that bucket.
  --
  -- To accomplish this, the list of weighted elements method is used, which is a method of
  -- specifying a list of elements with weights, and taking a random number, then taking a running
  -- sum of all of the weights of each element in the list. If the running sum is less than the
  -- random number, then the weight is added to the running sum and the next element is
  -- checked. Each succesive element is checked until the running sum exceeds the random number.
  --
  -- This function does the same, but with a mathematical "hack" to make it faster. The list of
  -- elements is the bucket along the X axis to choose, and the weight of each bucket is the
  -- gaussian of the X position of that bucket. So instead of taking a running sum through the list
  -- every time, we store the running sum of each bucket into a table (i.e. we take the discrete
  -- integral of the gaussian function). Then we binary search for which index in the table is
  -- closest to the random input variable. The expression:
  -- 
  -- @(prevStep <= 1 || prevValue == s)@
  --
  -- is the binary branch choice, which decides whether to move the search cursor up or down based
  -- on whether the running sum at the current table index is less than or greater the input
  -- 'Sample'. The search cursor starts in the middle of the table and has a step value which starts
  -- at a quarter the length of the table. The cursor jumps up or down by this step value, and after
  -- each jump, the step value is halved. The cursor continues to seek a value until the step size
  -- is less than or equal to 1, or in the event that the exact input 'Sample' value is found in the
  -- table (which is highly unlikely).
  --
  -- As a binary search, the complexity of this function is @O(log n)@ where @n@ is the length of
  -- the lookup table. A table size of 4096 == 2^12 elements is guaranteed to require at most 12
  -- table lookups for each evaluation of this function.

----------------------------------------------------------------------------------------------------

-- | Construct a permutation of a list. Pass an 'Prelude.Integer' permutation value and an
-- 'Prelude.Int' list length. The 'Prelude.Integer' permutation is found by multiplying the indicies
-- used to pull each element from the given list. For example, to create the permutation
-- @[4,2,0,1,3]@ from the list @[0,1,2,3,4]@:
--
-- @
--           |i|    |r|    |index 0 1 2 3 4|
-- take index 4 from 5 elements: [0,1,2,3,4] -> (4, [0,1,2,3])
-- take index 2 from 4 elements: [0,1,2,3]   -> (2, [0,1,3])
-- take index 0 from 3 elements: [0,1,3]     -> (0, [1,3])
-- take index 0 from 2 elements: [1,3]       -> (1, [3])
-- take index 0 from 1 elements: [3]         -> (3, [])
--            ^index ^radix                      ^resultant list
-- @
--
-- To create he above permutation, sum the product of each index and radix taken. Therefore, the the
-- first argument to the 'permute' function should be: @4*5 + 2*4 + 0*3 + 0*2 + 0*1 == 28@
permute :: Integer -> Int -> [a] -> [a]
permute perm len list = if null list then [] else if len<=0 then list else
  let (befor, after) = splitAt (fromIntegral $ mod perm $ toInteger len) list in
    head after : permute (div perm $ toInteger len) (len-1) (befor ++ tail after)

----------------------------------------------------------------------------------------------------

-- Miscelaneous utility functions

-- | Find the minimum and maximum element in an 'Mutable.STVector', evaluates to an error if the
-- vector is empty so this function is not total.
minMaxVec :: (Mutable.Unbox elem, Ord elem, Show elem {- TODO: delete Show -}) => Mutable.STVector s elem -> ST s (elem, elem)
minMaxVec vec = do
  let len = Mutable.length vec
  if len == 0 then error $ "minMaxVec called on empty vector" else do
    --traceM "compute (minimum, maximum) of vector..."
    init <- Mutable.read vec 0
    lo <- newSTRef init
    hi <- newSTRef init
    forM_ [1 .. len - 1] $ \ i ->
      Mutable.read vec i >>= \ elem -> modifySTRef lo (min elem) >> modifySTRef hi (max elem)
    id $ -- liftM (\ mm -> trace ("done, 'minMaxVec' returns " ++ show mm) $
      liftM2 (,) (readSTRef lo) (readSTRef hi)

-- | Given a minimum and maximum value, perform a simple linear transformation that normalizes all
-- elements in the given 'Mutable.STVector'.
normalize
  :: (Eq elem, Num elem, Fractional elem, Mutable.Unbox elem, Show elem {- TODO: delete Show -})
  => Mutable.STVector s elem -> (elem, elem) -> ST s ()
normalize vec (lo, hi) = do
  let offset = (hi + lo) / 2
  let scale  = (hi - lo) / 2
  --traceM $ "normalize vector around bounds (" ++ show lo ++ ", " ++ show hi ++
  --  "), offset = " ++ show offset ++ ", scale = " ++ show scale
  unless (scale == 0) $ forM_ [0 .. Mutable.length vec - 1] $ \ i ->
    Mutable.read vec i >>= Mutable.write vec i . (/ scale) . subtract offset

----------------------------------------------------------------------------------------------------

class (Eq n, Ord n, Num n) => HasTimeWindow a n | a -> n where
  timeWindow :: a -> TimeWindow n

-- | For discrete functions (like vectors), the 'timeEnd' value should be set to the length of the
-- vector. The 'twContains' function returns false if the index is equal to the 'timeEnd' value.
data TimeWindow t = TimeWindow{ timeStart :: !t, timeEnd :: !t }
  deriving (Eq, Ord, Show, Read, Functor)

-- | Returns 'Prelude.True' if the given point @t@ is contained within the -- 'TimeWindow'.
-- 'TimeWindow's are inclusive intervlas, meaning if the point @t@ is equal to either of the
-- 'timeStart' or 'timeEnd' values, this function returns 'Prelude.True'.
twContains :: Ord t => TimeWindow t -> t -> Bool
twContains (TimeWindow{ timeStart=t0, timeEnd=t1 }) t = t0 <= t && t < t1

-- | Shift the 'TimeWindow' forward or backward in time given the 'Duration' delta time value.
twShift :: Num t => t -> TimeWindow t -> TimeWindow t
twShift dt tw = TimeWindow{ timeStart = timeStart tw + dt, timeEnd = timeEnd tw + dt }

-- | Compute the 'Duration' of the 'TimeWindow'
twDuration :: Num t => TimeWindow t -> t
twDuration (TimeWindow{ timeStart=t0, timeEnd=t1 }) = t1 - t0

-- | Scale the 'Duration' of the 'TimeWindow' preserving the start time.
twScale :: Num t => t -> TimeWindow t -> TimeWindow t
twScale ts (TimeWindow{ timeStart=t0, timeEnd=t1 }) =
  TimeWindow{ timeStart=t0, timeEnd = t0 + ts * (t1 - t0) }

-- | Intersect two overlapping 'TimeWindow's. If the windows do not overlap, 'Prelude.Nothing' is
-- returned.
twIntersect :: Ord t => TimeWindow t -> TimeWindow t -> Maybe (TimeWindow t)
twIntersect (TimeWindow{ timeStart=a0, timeEnd=a1 }) (TimeWindow{ timeStart=b0, timeEnd=b1 }) =
  let aNeg = a0 > a1
      bNeg = b0 > b1
      aLo  = min a0 a1
      aHi  = max a0 a1
      bLo  = min b0 b1
      bHi  = max b0 b1
      neg  = if aNeg && not bNeg || not aNeg && bNeg then id else (\ (a,b) -> (b,a))
  in fmap ((\ (t0, t1) -> TimeWindow{ timeStart=t0, timeEnd=t1 }) . neg) $ case compare aLo bLo of
        EQ -> Just (aLo, min aHi bLo)
        LT -> if aHi < bLo then Nothing else Just (aHi, bLo)
        GT -> if bHi < aLo then Nothing else Just (bHi, aLo)

-- | Enumerate all time values in the givem 'TimeWindow' but with a given time-step value.
twParametric :: RealFrac t => t -> TimeWindow t -> [t]
twParametric step (TimeWindow{ timeStart=t0, timeEnd=t1 }) = do
  [realToFrac i * step + t0 | i <- [0::Int .. floor ((t1 - t0) / step)]]

-- | Similar to 'twParametric', except this function enumerate all time values in the given
-- 'TimeWindow' using the 'Prelude.pred' function in the 'Prelude.Enum' type class.
twEnum :: (Ord t, Enum t) => TimeWindow t -> [t]
twEnum (TimeWindow{ timeStart=t0, timeEnd=t1 }) =
  if t0 <= t1 then [t0 .. pred t1] else [t1 .. pred t0]

-- | Enumerate over a 'TimeWindow' of a quantized time-domain function using the 'twParametric'
-- function and passing the 'unitQuanta' as the time step.
twMoments :: TimeWindow Moment -> [Moment]
twMoments = twParametric unitQuanta

