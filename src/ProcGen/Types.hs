module ProcGen.Types
  ( module ProcGen.Types,
    module ProcGen.PrimeNumbers,
    Happlets.Audio.BufferSizeRequest,
    Happlets.Audio.AudioRealApprox,
    Happlets.Audio.Frequency,
    Happlets.Audio.Duration,
    Happlets.Audio.Moment,
    Happlets.Audio.Sample,
    Happlets.Audio.LeftSample,
    Happlets.Audio.RightSample,
    Happlets.Audio.PulseCode,
    Happlets.Audio.LeftPulseCode,
    Happlets.Audio.RightPulseCode,
    Happlets.Audio.SampleCount,
    Happlets.Audio.SampleIndex,
    Happlets.Audio.audioSampleRate,
    Happlets.Audio.unitQuanta,
    Happlets.Audio.maxFrequency,
    Happlets.Audio.minFrequency,
    Happlets.Audio.toPulseCode,
    Happlets.Audio.toSample,
    Happlets.Audio.modTimeIndex,
    Happlets.Audio.timeIndex,
    Happlets.Audio.indexToTime,
    Happlets.Audio.sampleCountDuration,
    Happlets.Audio.durationSampleCount,
    Happlets.Audio.mapTimeToStereo,
    Happlets.Audio.mapTimeToMono,
  ) where

import           ProcGen.PrimeNumbers

import           Control.Arrow                    ((&&&))
import           Control.Monad

import           Data.Ratio
import           Data.Semigroup
import qualified Data.Vector.Unboxed              as Unboxed
import qualified Data.Vector.Unboxed.Mutable      as Mutable
import           Data.Word

import           Happlets.Audio
import           Happlets.Lib.Gtk                 (gtkHapplet, theAnimationFrameRate)
import           Happlets.Provider                (defaultConfig)

----------------------------------------------------------------------------------------------------

data FuzzyParam
  = Not_at_all
  | A_little
  | Moderately
  | A_lot
  | Completely
  deriving (Eq, Ord, Show, Read, Enum)

----------------------------------------------------------------------------------------------------

-- | A single float type used by all other float types. Change this from Float to Double to change
-- the percision of all computations from 32 to 64-bit floating point values.
type ProcGenFloat = AudioRealApprox

type TimeScale   = ProcGenFloat
type Amplitude   = ProcGenFloat
type Wavelength  = ProcGenFloat
type PhaseShift  = ProcGenFloat
type Probability = ProcGenFloat
type HalfLife    = ProcGenFloat
type NoiseLevel  = ProcGenFloat
type Bandwidth   = ProcGenFloat
type Percentage  = ProcGenFloat

-- | Note that when you apply the 'TimeWindow', the remaining 'Envelope' type is synonymous with the
-- type @'Moment' -> 'Sample'@, and that this data type has an @instance@ for the 'TimeDomain'
-- class. Therefore, functions of type 'Envelope' data type instantiate the 'TimeDomain' class as
-- well, and can be used where ever there is a polymorphic type @td@ constrained by the typeclass
-- 'TimeDomain', as in
-- @'TimeDomain' td => ... -> td -> ...@.
--
-- As a law, the output of an 'Envelope' function must be on the inclusive interval
-- @[0 .. 1]@ for all input 'Moment's, although there is no type-level enforcement of this law.
type GenEnvelope n = TimeWindow n -> n -> n
type Envelope = GenEnvelope ProcGenFloat

-- | A 1-D continuous probability fistribution function
type CPDFunction = Moment -> Probability

-- | A 1-D continuous cumulative distribution function
type CCDFunction = Moment -> Probability

-- | A vector approximating a 'CCDFunction'. Table values of this type are constructed from a
-- 'CPDFunction' using the 'newIDSTable' function, and can be used by the 'inverseTransformSample'
-- function to generate random values "shaped" to the 'CPDFunction'.
type CumulativeDistributionTable = Unboxed.Vector Moment 

-- | This is a class to convert things that can be converted to 'Frequency' values, for example like
-- piano key values.
class HasFrequency f where
  toFreq :: f -> Frequency

-- | A class of time domain functions that can produce a 'Sample' from a 'Moment'.
--
-- Minimal complete definition: 'sample'.
class (Ord num, Num num, Fractional num) => TimeDomain f num where
  -- | Produce a sample at a single point in time for the given function @f@.
  sample :: f -> num -> num
  -- | Iterate over a range of moments, producing an integer number of samples. This function has a
  -- default implementation that calls 'sample' above, but for piecewise functions like splines it
  -- is much more efficient to iterate over the list of piecewise functions in a way that does not
  -- require searching through the entire list for a function that contains the 'Moment' at which
  -- you want a single 'Sample' repeatedly for each 'Moment' in the iteration.
  iterateSamples
    :: f -- ^ the function over which to iterate the samples points.
    -> Int -- ^ number of samples
    -> TimeWindow num -- ^ the time interval over which to iterate
    -> [(num, num)]
  iterateSamples = iterateSamplesWith . sample

-- | This is the function which is used to instantiate the 'iterateSamples' function of the
-- 'TimeDomain' typeclass by default if none is specified in the @instance@ for type @f@. This
-- function operates with any pure function over the domain of 'Moment's, not just functions which
-- instantiate the 'TimeDomain' typeclass.
iterateSamplesWith
  :: (Ord num, Num num, Fractional num)
  => (num -> num) -- ^ the function @f@ over which the range of 'Moment's is applied
  -> Int -- ^ the number of 'Moment' values to apply to the function @f@
  -> TimeWindow num -- ^ the time interval over which we iterate
  -> [(num, num)]
iterateSamplesWith f nelems win' = loop 0 where
  win      = twCanonicalize win'
  stepsize = twDuration win / realToFrac nelems
  loop   i = if i > nelems then [] else
    (id &&& f) (timeStart win + stepsize * realToFrac i) : loop (i + 1)

-- | A class of frequency domain functions that can produce an 'Amplitude' from a 'Frequency'.
class FreqDomain f where
  amplitude :: f -> Frequency -> Amplitude

-- | A type class for functions that have a 'Prelude.min', 'Prelude.max', 'Prelude.minimum', and
-- 'Prelude.maximum' implementation.
class Ord n => MinMaxDomain n where
  minOf :: n -> n -> n
  minOf = min
  maxOf :: n -> n -> n
  maxOf = max
  foldMin :: n -> [n] -> n
  foldMin = foldr minOf
  foldMax :: n -> [n] -> n
  foldMax = foldr maxOf

instance MinMaxDomain Int
instance MinMaxDomain Float
instance MinMaxDomain Double
instance MinMaxDomain Integer
instance (Integral n, MinMaxDomain n) => MinMaxDomain (Ratio n)

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

-- | Like 'toPulseCode' but evaluates to a 'Data.Word.Word16' value
toPulseCodeWord :: Sample -> Word16
toPulseCodeWord = fromIntegral . toPulseCode

-- | Like 'toSample' but takes a 'Data.Word.Word16' input instead of an 'Data.Int.Int16' input.
wordToSample :: Word16 -> Sample
wordToSample = toSample . fromIntegral

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

----------------------------------------------------------------------------------------------------

-- | Clamping values to a limit between 0 and 1, or between -1 and 1. There is no minimal complete
-- definition, all functions can be dervied with a stand-alone instance. This class exists to
-- provide an alternative lazy evaluation scheme for the 'RealValueFunction' data type.
class (Fractional n, MinMaxDomain n) => ClampedDomain n where
  -- | Clamp a value between 0 and 1.
  clamp0_1 :: n -> n
  clamp0_1 = min 1.0 . max 0.0

  -- | Clamp a value between -1 and 1.
  clamp1_1 :: n -> n
  clamp1_1 = min 1.0 . max (negate 1.0)

instance ClampedDomain Float
instance ClampedDomain Double

----------------------------------------------------------------------------------------------------

-- | Modulous over real number approximating data types i.e. the 'Float' and 'Double' data
-- types). There is no minimal complete definition, all functions can be dervied with a stand-alone
-- instance. This class exists to provide an alternative lazy evaluation scheme for the
-- 'RealValueFunction' data type.
class ModulousDomain n where
  -- | Modulous over a continuous value, implemented in terms of the 'Prelude.round' function.
  contMod  :: n -> n -> n

instance ModulousDomain Float    where { contMod = realFracContMod; }
instance ModulousDomain Double   where { contMod = realFracContMod; }
instance ModulousDomain Rational where { contMod = realFracContMod; }

-- | A generalized version of 'contMod' which works for any 'Ord' 'RealFrac' data type.
realFracContMod :: forall n . (Ord n, RealFrac n) => n -> n -> n
realFracContMod a b = mod (if a<0 then negate else id)
  (if b<0 then ceiling :: n -> Integer else floor :: n -> Integer)
  (abs a) (abs b)
  where
    mod negate round a b = negate $ a - realToFrac (round (a / b)) * b

----------------------------------------------------------------------------------------------------

-- | Rounding functions for real number approximating data types i.e. the 'Float' and 'Double' data
-- types). There is no minimal complete definition, all functions can be dervied with a stand-alone
-- instance. This class exists to provide an alternative lazy evaluation scheme for the
-- 'RealValueFunction' data type.
class RoundingDomain n where
  -- | Round to the 'floor'.
  roundDown :: n -> n
  -- | Round to the 'ceiling'
  roundUp   :: n -> n
  -- | Round down if below the mid-way point between between the floor and celing, round up
  -- otherwise.
  roundMid  :: n -> n

fracRoundDown :: forall n . RealFrac n => n -> n
fracRoundDown = realToFrac . (floor :: n -> Integer)

fracRoundUp :: forall n . RealFrac n => n -> n
fracRoundUp = realToFrac . (ceiling :: n -> Integer)

fracRoundMid :: forall n . RealFrac n => n -> n
fracRoundMid = realToFrac . (round :: n -> Integer)

instance RoundingDomain Float where
  roundDown = fracRoundDown
  roundUp   = fracRoundUp
  roundMid  = fracRoundMid

instance RoundingDomain Double where
  roundDown = fracRoundDown
  roundUp   = fracRoundUp
  roundMid  = fracRoundMid

instance RoundingDomain Rational where
  roundDown = fracRoundDown
  roundUp   = fracRoundUp
  roundMid  = fracRoundMid

instance RoundingDomain Int where
  roundDown = id
  roundUp   = id
  roundMid  = id

instance RoundingDomain Integer where
  roundDown = id
  roundUp   = id
  roundMid  = id

----------------------------------------------------------------------------------------------------

-- | Periodic fundamental wave functions for real number approximating data types i.e. 'Float' and
-- 'Double'. All of these functions have a period of exacly 1 unit. There is no minimal complete
-- definition, all functions can be dervied with a stand-alone instance. This class exists to
-- provide an alternative lazy evaluation scheme for the 'RealValueFunction' data type.
class PeriodicDomain n where
  -- | A time domain function computing unit sine wave function, where the period of the sineusoid is
  -- 1, rather than 2π.
  unitSine :: n -> n
  -- | A time domain function computing periodic unit sawtooth function.
  sawtooth :: n -> n
  -- | A time domain function computing periodic unit triangular function.
  triangle :: n -> n
  -- | A time domain function computing periodic unit triangular function.
  square   :: n -> n

instance PeriodicDomain Float where
  unitSine = floatUnitSine
  sawtooth = floatSawtooth
  triangle = floatTriangle
  square   = floatSquare

instance PeriodicDomain Double where
  unitSine = floatUnitSine
  sawtooth = floatSawtooth
  triangle = floatTriangle
  square   = floatSquare

-- | A generalized version of 'unitSine' that works for any 'Floating' 'RealFrac' value.
floatUnitSine ::  (Floating n, RealFrac n) => n -> n
floatUnitSine = sin . ((2*pi) *)

-- | A generalized version of 'sawtooth' that works for any 'Floating' 'RealFrac' value.
floatSawtooth ::  (Floating n, RealFrac n) => n -> n
floatSawtooth t0 = let t = 2*t0 + 0.5 in 2*(t - realToFrac (floor t :: Integer)) - 1

-- | A generalized version of 'triangle' that works for any 'Floating' 'RealFrac' value.
floatTriangle ::  (Floating n, RealFrac n) => n -> n
floatTriangle t0 = let t = 2*t0 + 0.5 in 2*(abs $ floatSawtooth t) - 1

-- | A generalized version of 'square' that works for any 'Floating' 'RealFrac' value.
floatSquare ::  (Floating n, RealFrac n) => n -> n
floatSquare t0 = (if t0 - (realToFrac (round t0 :: Int)) < 0.5 then id else negate) 1

----------------------------------------------------------------------------------------------------

-- | Envelope functions for real number approximating data types, i.e. 'Float' and 'Double'. There
-- is no minimal complete definition, all functions can be dervied with a stand-alone instance. This
-- class exists to provide an alternative lazy evaluation scheme for the 'RealValueFunction' data
-- type.
class (Ord n, Floating n) => EnvelopeDomain n where
  -- | An 'Envelope' increasing linearly from zero to one starting at a given 'Moment' and
  -- increasing to one over a period of 'TimeSpan' amount of time. This function can be thought of
  -- as 'relu'-like function that is parameterized like an 'Envelop' function.
  --
  -- A negative 'TimeSpan' indicates the envelope should go from one to zero.
  slope :: Floating n => GenEnvelope n
  slope (TimeWindow{timeStart=t0,timeEnd=t1}) t = let tw = t1-t0 in case compare tw 0 of
    EQ -> if t< t0 then 0 else 1
    LT -> if t<=t0 then 1 else if t>=t0 then 0 else 1 + (t-t0)/tw
    GT -> if t<=t0 then 0 else if t>=t1 then 1 else (t-t0)/tw

  -- | An 'Envelope' increasing over gradual a sigmoidal function from zero to one starting at a
  -- given 'Moment' and increasing to one over a period of 'TimeSpan' amount of time.
  --
  -- A negative 'TimeSpan' indicates the envelope should go from one to zero. The sigmoidal function
  -- is approximated by a polynomial, so it is not the genuine equation for the sigmoid which
  -- computes an exponent of the natural base value @e@, but the approximation is more than close
  -- enough and it also ensures the slope of the unit curve at t=0 and t=1 is exactly zero.
  --
  -- This function could certainly be in the 'ActivationDomain', but in this form it follows the
  -- parameterization of an 'Envelope' function.
  sigmoid :: GenEnvelope n
  sigmoid win@(TimeWindow{timeStart=t0,timeEnd=t1}) t' =
    let tw = t1 - t0
        t = slope win t'
        n = 4 -- n must be even
    in  if tw==0 then if t'<t0 then 0 else 1 else
        if t<=0.5 then (2*t)**n/2 else 1 - (2*t - 2)**n/2

  -- | An 'Envelope' increasing over the square of the sine of @t@.
  sineSquared :: GenEnvelope n
  sineSquared win@(TimeWindow{timeStart=t0,timeEnd=t1}) t' =
    let { tw = t1 - t0; t = slope win t'; } in
    if tw==9 then if t'<t0 then 0 else 1 else sin(pi*t/2)**2

  -- | A function that constructs a fade-in and fade-out envelope.
  fadeInOut
    :: n -- ^ t0
    -> n -- ^ t1 (fade-in occurs betwen t0 and t1)
    -> n -- ^ t2 (no fading occurs between t1 and t2)
    -> n -- ^ t3 (fade-out occurs between t2 and t3)
    -> n -> n
  fadeInOut t0 t1 t2 t3 t = sigmoid TimeWindow{ timeStart = t0, timeEnd = t1 } t *
    (1.0 - sigmoid TimeWindow{ timeStart = t2, timeEnd = t3 } t)

instance EnvelopeDomain Float
instance EnvelopeDomain Double

----------------------------------------------------------------------------------------------------

-- | A domain of sinusoidal functions that have been multiplied by an envelope function to restrict
-- their image to a single pulse shape. This function is a type class over real number approximating
-- data types, i.e. 'Float' and 'Double'. There is no minimal complete definition, all functions can
-- be dervied with a stand-alone instance. This class exists to provide an alternative lazy
-- evaluation scheme for the 'RealValueFunction' data type.
class
  (Ord n, Floating n, ProbabilityDomain n, EnvelopeDomain n)
  => PulsedSinusoidalDomain n where
    -- | An function which produces a single cycle of a sinusoidal pulse of the given frequency and a
    -- time offset moment. The pulse is a sineusoid enveloped by the Gaussian normal function, which
    -- is produces a slightly distorted sinusoud with a duration of a single cycle.
    sinePulse
      :: n -- ^ Frequency
      -> n -- ^ PhaseShift
      -> n -> n
    sinePulse freq t0 t = normal 1.0 t * sin (2 * pi * freq * t + t0)

    -- | Like a 'sinePulse' but produces three cycles: 1 cycle with a sigmoidal ramp-up envelope, 1
    -- cycle at a constant 1.0 envelope, and 1 cycle with a sigmoidal ramp-down envelope. Note that
    -- the name of this function has to do with the fact that it creates a sine pulse of three cycles,
    -- not that this is a 3rd kind of 'sinePulse' function (there is no @sinePulse2@ function).
    sinePulse3
      :: n -- ^ Frequency of the sine wave, also determines the size of the pulse equal to 3 times
           -- the inverse of the frequency.
      -> n -- ^ The start time of the pulse, where the signal ramps-up.
      -> n -- ^ The phase shift of the sine wave relative to the above pulse start time.
      -> n -> n
    sinePulse3 freq t0 phase t = sin (2 * pi * freq * (t0 + phase + t)) *
      fadeInOut t0 (t0 + 1/freq) (t0 + 2/freq) (t0 + 3/freq) t

instance PulsedSinusoidalDomain Float
instance PulsedSinusoidalDomain Double

----------------------------------------------------------------------------------------------------

-- | A class of activation functions (a class of functions used in computing artificial neural
-- networks, although this function does not include 'atan') for real value approximating data types
-- i.e. 'Float' and 'Double'. There is no minimal complete definition, all functions can be dervied
-- with a stand-alone instance. This class exists to provide an alternative lazy evaluation scheme
-- for the 'RealValueFunction' data type.
--
-- The 'sigmoid' function could certainly go here, but because it's formulation matches that of an
-- 'Envelop'ed function, it is not a member of this type class but of the 'EnvelopeDomain' type
-- class.
class (Ord n, Floating n) => ActivationDomain n where
  -- | Given 4 control points, compute the curve value for a single parametric value.
  bezier3 :: n -> n -> n -> n -> n -> n
  bezier3 a b c d t = a*(1-t)**3 + b*3*t*(1-t)**2 + c*3*(1-t)*t**2 + d*t**3

  -- | The ReLU function, is an activation function used in neural networks which is defined as
  -- @(\\t -> if t < 0 then 0 else t)@
  relu :: n -> n
  relu t = if t < 0 then 0 else t

  -- | Like 'ReLU' but evaluates to a constant @c@ rather than 0 if the input @t@ is less than
  -- @c@. Usually @c@ is very small, like @0.001@
  leakyReLU :: n -> n -> n
  leakyReLU c t = if t < c then c else t

instance ActivationDomain Float
instance ActivationDomain Double

-- | Sum several 'sinePulse's together. This is pretty inefficient, since each 'Sample' produced
-- requires O(n) time to compute where @n@ is the length of the number of components. However for
-- simpler functions with a few components, this function is useful.
sinePulseSum :: PulsedSinusoidalDomain n => [(n, n)] -> n -> n
sinePulseSum comps t = case comps of
  [] -> 0
  (freq, t0):comps -> sinePulse freq t0 t + sinePulseSum comps t

class Floating n => ProbabilityDomain n where
  -- | A normal normal curve defined as @\\x -> e^(-(2*x)^2)@, which has a variance of @e@ so the
  -- curve fits pretty nicely within the range between -1.0 and 1.0.
  normal
    :: n -- ^ TimeScale
    -> n -> n
  normal var x = exp $ negate $ x * x * exp 2 / var * var

  -- | Create a Beta distribution curve of rank @n@ where @n is the first paramter passed to this
  -- function. The curve is not limited to the range between 'Moment's 0 and 1, however it is mostly
  -- on this interval between 0 and 1 where this function is useful. The rank is compareable to the
  -- rank of a polynomial, however non-integer values may be passed for exponentiation. The
  -- reciporical of the rank defines where the peak value lies, that is for which value of @n@ such
  -- that @(beta n (1/n)) == 1.0@.
  beta
    :: n -- ^ polynomial rank
    -> n -> n
  beta rank x = let n = rank - 1 in rank**rank * x * (1 - x)**n / n**n

instance ProbabilityDomain Float
instance ProbabilityDomain Double

-- | Create a continuous time domain function from a discrete unboxed 'Unboxed.Vector' of values by
-- converting from a real-number value to an integer vector index, with linear smoothing for values
-- between vector elements (which means, for example, an index of 0.5 will be the average of index 0
-- and index 1).
continuous
  :: Sample -- ^ The value of the function for values that exist before the vector.
  -> Sample -- ^ The value of the function for values that exist after the vector.
  -> TimeScale -- ^ A scalar value that will be multipled times every input time value which converts
               -- integer indicies to real-number indicies.
  -> Unboxed.Vector Sample -> Moment -> Sample
continuous before after scale vec t =
  if i + 1 < 0 then before else if len <= i then after else lo + d * (hi - lo) where
    st  = scale * t
    i   = floor st
    d   = st - realToFrac i
    len = Unboxed.length vec
    (lo, hi) =
      if i + 1 == 0   then (before, vec Unboxed.! 0) else
      if i + 1 == len then (vec Unboxed.! i, after)  else
        (vec Unboxed.! i, vec Unboxed.! (i + 1))

-- | A function for generating a 'Unboxed.Vector' that can contains a discrete approximation of the
-- integral of any continuous probability distribution function ('CPDFunction'), which can be used
-- with the 'inverseTransformSample' function to lookup an index associated with a probability. This
-- function can be used to shape randomly generated numbers so that they tend more toward a normal
-- distribution. Simply request the size of the table to generate, the larger the table, the more
-- accurate the inverse function will be. 4096 elements should be plenty for most purposes. If you
-- are concerned about performance you may also want to keep in mind the physical CPU hardware this
-- program is running on, and what the size of available level-2/level-3 cache is, and choose a
-- table size that will fit within the cache.
newITSTable :: CPDFunction -> Int -> Unboxed.Vector Moment
newITSTable cpdf size = Unboxed.create $ do
  mvec <- Mutable.new size
  let f s i = do
        Mutable.write mvec i s
        return $ s + cpdf (realToFrac i / realToFrac size)
  maxsamp <- foldM f (0.0) [0 .. size - 1]
  mapM_ (\ i -> Mutable.read mvec i >>= Mutable.write mvec i . (/ maxsamp)) [0 .. size - 1]
  return mvec

-- | Compute the inverse transform sample function, the computation is performed with a
-- 'Unboxed.Vector' created by the 'newITSTable' function. The result of this function is always an
-- index value in the given 'CumulativeDistributionTable' divided by the size of the
-- 'CumulativeDistributionTable', which guarantees the result is a value between 0 and 1. When a
-- random value between 0 and 1 is selected as an input 'Sample' to this function, the output random
-- value is going to be shifted such that it is more likely to lie near a high point on the
-- probability distribution function, and less likely to lie near a low point.
--
-- For example, for a 'inverseNormalTable' (which approximates the cumulative distribution function
-- table for the 'normal' function), most of the values resulting from this function will be near to
-- @1/2@, and there will almost never be a value near 0 or near 1. As another example, when
-- evaluating this function on random value with the 'inverseBeta5' table, most random outputs will
-- be nearest the value @1/5@, and values near 1.0 will almost never happen.
--
-- This function uses a binary search to traverse the 'CumulativeDistributionTable', so the time
-- compexity of this function is @O(log n)@ where @n@ is the size of the
-- 'CumulativeDistributionTable'. For a table of 4096 == 2^12 elements, this function is guaranteed
-- to execute no more than 12 table lookups.
inverseTransformSample :: CumulativeDistributionTable -> Sample -> Moment
inverseTransformSample table s = if 0.0 <= s && s <= 1.0 then loop i0 i0 $ lookup i0 else
  error $ "(inverseTransformSample "++show s++") error: must be evaluated on value beween 0 and 1"
  where
    lookup = (table Unboxed.!)
    size   = Unboxed.length table
    i0     = div size 2
    loop prevStep i0 prevVal = let absPrevStep = abs prevStep in
      if absPrevStep <= 1 || prevVal == s then realToFrac i0 / realToFrac size else
        let nextStep = (if prevVal > s then negate else id) $ div absPrevStep 2
            i        = i0 + nextStep
        in  loop nextStep i $ lookup i
    -- This algorithm works by using a table for values of the continuous cumulative distribution
    -- function. Suppose it takes a random 'Sample' input between 0 and 1, and a sequence of descrete
    -- buckets along the X axis. This purpose of this function is to adjust all random 'Sample's so
    -- that the odds of a sample landing in a discrete bucket along the X axis is about equal to the
    -- value of the normal curve for the X position of that bucket.
    --
    -- To accomplish this, the list of weighted elements method is used, which is a method of
    -- specifying a list of elements with weights, and taking a random number, then taking a running
    -- sum of all of the weights of each element in the list (this is also called a Cumulative
    -- Distribution Function). If the running sum is less than the random number, then the weight is
    -- added to the running sum and the next element is checked. Each succesive element is checked
    -- until the running sum exceeds the random number.
    --
    -- This function does the same, but with a mathematical "hack" to make it faster. The list of
    -- elements is the bucket along the X axis to choose, and the weight of each bucket is the normal
    -- of the X position of that bucket. So instead of taking a running sum through the list every
    -- time, we store the running sum of each bucket into a table (i.e. we take the discrete integral
    -- of the probability distribution function). Then we binary search for which index in the table
    -- is closest to the random input variable. The expression:
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

-- | The most commonly used probability distribution function is the Gaussian 'normal'
-- function. Since it is so common, a default cumulative distribution table for the normal
-- distribution is provided here, which is equal to @('newITSTable' ('normal' 1.0) 4096)@.
inverseNormalTable :: CumulativeDistributionTable
inverseNormalTable = newITSTable (normal 1.0 . subtract 1.0 . (2.0 *)) 4096

-- | The most commonly used probability distribution function is the Gaussian 'normal'
-- function. Since it is so common, a default inverse transofmr sample function for the normal
-- distribution is provided here, which is equal to a memoized version of the expression
-- @(inverseTransformSample ('newITSTable' ('normal' 1.0) 4096))@.
inverseNormal :: Sample -> Moment
inverseNormal = inverseTransformSample inverseNormalTable

-- | Another often used probability distribution is the Beta distribution, which is a polynomial
-- favoring values closest to the point @1/n@ where @n@ is the rank of the polynomial. This table
-- approximates the cumulative distribution function for a Beta distribution polynomial of rank 5,
-- thus the most probable values produced by the 'inverseBeta5' are values around @1/5@.
inverseBeta5Table :: CumulativeDistributionTable
inverseBeta5Table = newITSTable (beta 5) 4096

-- | Another often used probability distribution is the Beta distribution, which is a polynomial
-- favoring values closest to the point @1/n@ where @n@ is the rank of the polynomial. This table
-- approximates the cumulative distribution function for a Beta distribution polynomial of rank 5,
-- thus the most probably values produced by the 'inverseBeta5' are values around @1/5@.
inverseBeta5 :: Sample -> Moment
inverseBeta5 = inverseTransformSample inverseBeta5Table

inverseBeta3Table :: CumulativeDistributionTable
inverseBeta3Table = newITSTable (beta 3) 4096

inverseBeta3 :: Sample -> Moment
inverseBeta3 = inverseTransformSample inverseBeta3Table

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

-- | A function with strict stateful minimum and maximum values.
data MinMax n = MinMax{ minValue :: !n, maxValue :: !n }
  deriving (Eq, Show, Read)

instance MinMaxDomain n => Semigroup (MinMax n) where
  (MinMax a b) <> (MinMax c d) = MinMax
    (min a $ min b $ min c d)
    (max a $ max b $ max c d)

instance (MinMaxDomain n, Bounded n) => Monoid (MinMax n) where
  mempty = MinMax{ minValue = maxBound, maxValue = minBound }
  mappend = (<>)

stepMinMax :: MinMaxDomain n => MinMax n -> n -> MinMax n
stepMinMax (MinMax lo hi) i = MinMax (min lo i) (max hi i)

minMax :: (Bounded n, MinMaxDomain n) => [n] -> MinMax n
minMax = foldl stepMinMax mempty

----------------------------------------------------------------------------------------------------

class (Eq n, Ord n, Num n) => HasTimeWindow a n | a -> n where
  timeWindow :: a -> Maybe (TimeWindow n)

-- | For discrete functions (like vectors), the 'timeEnd' value should be set to the length of the
-- vector. The 'twContains' function returns false if the index is equal to the 'timeEnd' value.
data TimeWindow t = TimeWindow{ timeStart :: !t, timeEnd :: !t }
  deriving (Eq, Ord, Show, Read, Functor)

instance HasTimeWindow (TimeWindow Moment) Moment where { timeWindow = Just; }
instance HasTimeWindow (TimeWindow Int   ) Int    where { timeWindow = Just; }
instance HasTimeWindow [TimeWindow Moment] Moment where { timeWindow = twMinBoundsAll; }
instance HasTimeWindow [TimeWindow Int   ] Int    where { timeWindow = twMinBoundsAll; }

-- | Canonicalize the 'TimeWindow' value, which means this function evaluates to 'TimeWindow' value
-- such that the 'timeStart' value is always less than or equal to the 'timeEnd' value.
twCanonicalize :: Ord t => TimeWindow t -> TimeWindow t
twCanonicalize (TimeWindow{timeStart=t0,timeEnd=t1}) = TimeWindow{ timeStart = t1, timeEnd = t0 }

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
        EQ -> Just (aLo, Prelude.min aHi bLo)
        LT -> if aHi < bLo then Nothing else Just (aHi, bLo)
        GT -> if bHi < aLo then Nothing else Just (bHi, aLo)

-- | Compute the minimum bounding window required to fit both windows given. This is like a union of
-- two windows except the windows need not overlap.
twMinBounds :: Ord t => TimeWindow t -> TimeWindow t -> TimeWindow t
twMinBounds a b =
  TimeWindow{ timeStart = Prelude.minimum times, timeEnd = Prelude.maximum times } where
    times = [timeStart a, timeEnd a, timeStart b, timeEnd b]

twMinBoundsAll :: (Eq n, Ord n, Num n) => [TimeWindow n] -> Maybe (TimeWindow n)
twMinBoundsAll = \ case
  []   -> Nothing
  a:ax -> Just $ foldl twMinBounds a ax

-- | Enumerate all time values in the givem 'TimeWindow' but with a given time-step value.
twParametric :: RealFrac t => t -> TimeWindow t -> [t]
twParametric step (TimeWindow{ timeStart=t0, timeEnd=t1 }) = do
  [realToFrac i * step + t0 | i <- [0::Int .. floor ((t1 - t0) / step)]]

-- | Similar to 'twParametric', except this function enumerate all time values in the given
-- 'TimeWindow' using the 'Prelude.succ' function in the 'Prelude.Enum' type class. The 'timeEnd'
-- element is not included in the iteration.
--
-- Use this function when iterating over a buffer of 'Sample's.
twEnum :: (Ord t, Enum t) => TimeWindow t -> [t]
twEnum (TimeWindow{ timeStart=t0, timeEnd=t1 }) =
  if t0 <= t1 then [t0 .. pred t1] else [t1 .. pred t0]

-- | Enumerate over a 'TimeWindow' of a quantized time-domain function using the 'twParametric'
-- function and passing the 'unitQuanta' as the time step.
twMoments :: TimeWindow Moment -> [Moment]
twMoments = twParametric unitQuanta

-- | Similar to 'twMoments' but maps the results with @('timeIndex')@
twIterate :: TimeWindow Moment -> [(Int, ProcGenFloat)]
twIterate = fmap modTimeIndex . twMoments

-- | Similar to 'twIterate' but is designed for use with vectors, this function does bounds checking
-- on the vector once to make sure the 'TimeWindow' given does not exceed the bounds of the
-- vector. Pass the length of the vector as the first parameter.
twIndicies :: Int -> TimeWindow Moment -> [Int]
twIndicies len (TimeWindow{timeStart=t0,timeEnd=end}) =
  [Prelude.max 0 $ Prelude.min (len - 1) $ durationSampleCount t0 ..
   Prelude.min (len - 1) $ durationSampleCount end]
