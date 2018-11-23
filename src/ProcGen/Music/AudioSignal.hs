-- | This module defines the data types and functions for creating sound effects that can be used as
-- musical instruments.
module ProcGen.Music.AudioSignal
  ( -- * Buffering
    BufferIDCT(..), newSampleBuffer,
    -- * Elements of frequency domain functions
    NormFrequency, freqToNorm, normToFreq, freqToPercent, percentToNorm,
    FreqCoeficient(..), freqCoeficient,
    FDComponent, emptyFDComponent,
    fdFreqCoef, fdAmplitude, fdPhaseShift, fdDecayRate,
    fdNoiseLevel, fdUndertone, fdUnderphase, fdUnderamp,
    FDComponentList, fdComponentList, fdComponentInsert,
    forEachFDComponent, forEachFDComponent_,
    fdComponentSampleAt, fdComponentAmplitudeAt,
    -- * Signals as frequency domain functions
    FDSignal, fdSignal, fdSignalVector, emptyFDSignal, nullFDSignal,
    theFDSize, theFDMinFreq, theFDMaxFreq, theFDBaseFreq,
    listFDElems, listFDAssocs, lookupFDComponent,
    componentMultipliers,
    -- * Time Domain Function Construction
    TDSignal, tdSamples, allTDSamples, listTDSamples, tdTimeWindow, tdDuration, pureIDCT,
    minMaxTDSignal, writeTDSignalFile, readTDSignalFile,
    -- * Graphical Representations of Functions
    FDView(..), fdView, runFDView,
    TDView(..), tdView, runTDView,
    --tdViewInitTime, tdViewAnimate, tdViewFrameCount, tdViewAtTime,
    --resizeTDView, animateTDView, clickMouseTDView, drawTDView,
    CreateRandomFD(..), DrawingMode(..),
  ) where

import           Happlets.Lib.Gtk

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Buffer
--import           ProcGen.Collapsible
import           ProcGen.Music.WaveFile
import           ProcGen.Properties
--import           ProcGen.VectorBuilder

import           Control.Arrow
import           Control.Monad.ST

import           Data.Semigroup
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable

import           Linear.V2

import qualified Graphics.Rendering.Cairo    as Cairo

import           Text.Printf

----------------------------------------------------------------------------------------------------

-- | This is the class of functions that can perform an __I__nverse __D__iscrete __C__osine
-- __T__ransform on some functional data type like 'FDSignal' or 'FDComponent', rendering the
-- results to some mutable buffer with unboxed 'ProcGen.Types.Sample' elements.
--
-- Note that the instances of this class are not expected to perform any normalization on the
-- signals rendered into the buffer. Normalization should happen as the final step of creating a
-- buffer.
class BufferIDCT fdsig where
  bufferIDCT
    :: PrimMonad m
    => Mutable.MVector (PrimState m) Sample -> TimeWindow Moment -> fdsig -> TFRandT m ()

-- | Create a new buffer large enough to store 'ProcGen.Types.Duration' seconds worth of
-- 'ProcGen.Types.Sample's. Note that the return type of this function is polymorphic and can be
-- unified with either a type of 'Mutable.STVector' or an 'Mutable.IOVector'.
newSampleBuffer :: PrimMonad m => Duration -> m (Mutable.MVector (PrimState m) Sample)
newSampleBuffer = Mutable.new . (+ 1) . durationSampleCount

----------------------------------------------------------------------------------------------------

-- | Used to count the number of component frequencies in an 'FDSignal'
type ComponentCount = Int

-- | Used to select a single component frequency from an 'FDSignal'
type ComponentIndex = Int

-- | This value stores a 'Freqency' that is understood to be in the audible range between 15.0 Hz
-- and 22.1 KHz. The value stored within this constructor is a 'ProcGenFloat' between the value of 0
-- and 1. When extracting this value, a result in the audible range is produced. When constructing
-- this value, you must specify whehter the value is normalized or not using 'percentToNorm' or
-- 'freqToNorm'. Extract the value using 'normToFreq'
newtype NormFrequency = NormFrequency ProcGenFloat
  deriving (Eq, Ord)

instance Show NormFrequency where { show = show . normToFreq; }

-- | Pass a value between 0 and 1 to construct a normalized frequency. This function is the inverse
-- of 'freqToPercent'.
percentToNorm :: Percentage -> NormFrequency
percentToNorm f = NormFrequency $ max 0 $ min 1.0 f

-- | Extract a normalized value between 0 and 1. This function is the inverse of 'percentToNorm'.
freqToPercent :: NormFrequency -> Percentage
freqToPercent (NormFrequency f) = f

-- | Extract an ordinary, non-normalized 'Frequency' value from the 'NormFrequency'.
normToFreq :: NormFrequency -> Frequency
normToFreq (NormFrequency f) = f * (nyquist - minFrequency) + minFrequency

-- | Pass an ordinary, non-normalized 'Freuquency' value in order to store it as a normalized
-- value. 'Frequency' values beyond the audible range are truncated to the limits of audible
-- frequency, then normalized.
freqToNorm :: Frequency -> NormFrequency
freqToNorm f = NormFrequency $
  (max minFrequency (min nyquist f) - minFrequency) / (nyquist - minFrequency)

----------------------------------------------------------------------------------------------------

-- | A frequency coeficient is a value that will be multiplied by a base 'Frequency' to produce a
-- new frequency. This is necessary because the 'FDSignal' data type stores component values as a
-- multiple of some base frequency. This makes it easier to compute similar sounds at different
-- frequencies by simply changing the base 'Frequency'. All the actual frequency values passed to
-- the IDCT are computed by multiplying the base 'Frequenc' by some coeficient.
newtype FreqCoeficient = FreqCoeficient { unwrapFreqCoef :: ProcGenFloat }
  deriving (Eq, Ord, Show)

-- | Unwrap a 'FreqCoeficient' and multiply it times a given base frequency.
freqCoeficient :: NormFrequency -> FreqCoeficient -> Frequency
freqCoeficient normf (FreqCoeficient c) = c * normToFreq normf

----------------------------------------------------------------------------------------------------

data FDComponent
  = FDComponent
    { theFDFreqCoef   :: !FreqCoeficient  -- ^ must be between 15.0 and 22100.0 Hz
    , theFDAmplitude  :: !Amplitude 
    , theFDPhaseShift :: !PhaseShift -- ^ must be between 0 and 1, automatically scaled to 2*pi
    , theFDDecayRate  :: !HalfLife   -- ^ set to zero for no decay
    , theFDNoiseLevel :: !NoiseLevel
      -- ^ how much noise to apply, as a measure of variance in the 'fdAmplitude'. A value of @1.0@
      -- means the amplitude of each cycle varies is multiplied by a random number between @0.0@ to
      -- @1.0@. A value of @0.5@ means the amplitude of each cycle is mulitplied by a random number
      -- between @0.5@ (half volume) and @1.0@ (full volume).
    , theFDUndertone  :: !Frequency
      -- ^ set whehther the amplitude is also oscillating, must be between 0.0 and 7.5 Hz, zero
      -- indicates no oscillation. Also if the 'fdDecayRate' has a half life less than the period of
      -- the 'fdUndertone', the 'fdUndertone' is ignored.
    , theFDUnderphase :: !PhaseShift
      -- ^ the phase shift for the 'fdUndertone'
    , theFDUnderamp   :: !Amplitude
      -- ^ the amplitude of the 'fdUndertone'
    }
  deriving (Eq, Ord)

fdFreqCoef  :: Lens' FDComponent FreqCoeficient
fdFreqCoef = lens theFDFreqCoef $ \ a b -> a{ theFDFreqCoef = b }

fdAmplitude  :: Lens' FDComponent Amplitude
fdAmplitude = lens theFDAmplitude $ \ a b -> a{ theFDAmplitude = b }

fdPhaseShift :: Lens' FDComponent PhaseShift
fdPhaseShift = lens theFDPhaseShift $ \ a b -> a{ theFDPhaseShift = b }

fdDecayRate  :: Lens' FDComponent HalfLife
fdDecayRate = lens theFDDecayRate $ \ a b -> a{ theFDDecayRate = b }

fdNoiseLevel :: Lens' FDComponent NoiseLevel
fdNoiseLevel = lens theFDNoiseLevel $ \ a b -> a{ theFDNoiseLevel = b }

fdUndertone  :: Lens' FDComponent Frequency
fdUndertone = lens theFDUndertone $ \ a b -> a{ theFDUndertone = b }

fdUnderphase :: Lens' FDComponent PhaseShift
fdUnderphase = lens theFDUnderphase $ \ a b -> a{ theFDUnderphase = b }

fdUnderamp   :: Lens' FDComponent Amplitude
fdUnderamp = lens theFDUnderamp $ \ a b -> a{ theFDUnderamp = b }

instance Show FDComponent where
  show fd = printf
    "(FD freq=%+.4f amp=%+.4f phase=%+.4f decay=%+.4f noise=%+.4f undrtone=%+.4f undrphse=%.4f)"
    (unwrapFreqCoef $ fd ^. fdFreqCoef)
    (fd ^. fdAmplitude) (fd ^. fdPhaseShift) (fd ^. fdDecayRate)
    (fd ^. fdNoiseLevel) (fd ^. fdUndertone) (fd ^. fdUnderphase)

--instance Collapsible Float FDComponent where
--  collapse =
--    buildRecord ((\ (NormFrequency f) -> f) . theFDFreqCoef)  <>
--    buildRecord theFDAmplitude  <>
--    buildRecord theFDPhaseShift <>
--    buildRecord theFDDecayRate  <>
--    buildRecord theFDNoiseLevel <>
--    buildRecord theFDUndertone  <>
--    buildRecord theFDUnderphase <>
--    buildRecord theFDUnderamp
--  uncollapse = error "TODO: (uncollapse :: UVec.Vector -> FDComponent)"

emptyFDComponent :: FDComponent
emptyFDComponent = FDComponent
  { theFDFreqCoef   = FreqCoeficient 0
  , theFDAmplitude  = 0
  , theFDPhaseShift = 0
  , theFDDecayRate  = 0
  , theFDNoiseLevel = 0
  , theFDUndertone  = 0
  , theFDUnderphase = 0
  , theFDUnderamp   = 0
  }

-- | Returns 'Prelude.True' if either th frequency or amplitude are zero.
nullFDComponent :: FDComponent -> Bool
nullFDComponent fd = unwrapFreqCoef (fd ^. fdFreqCoef) == 0 || fd ^. fdAmplitude == 0

-- | Computes the exact 'ProcGen.Types.Sample' value at a given time produced by this
-- component. This function cannot make use of the 'fdNoiseLevel' value of the 'FDComponent',
-- because this is a pure function that has no access to a random number generator.
fdComponentSampleAt :: NormFrequency -> FDComponent -> Moment -> Sample
fdComponentSampleAt base fd t = if freq > nyquist then 0 else
  fdComponentAmplitudeAt fd t * sin (freq * 2.0 * pi * (t + (fd ^. fdPhaseShift)))
  where
    freq = freqCoeficient base (fd ^. fdFreqCoef)

-- | Like 'fdComponentSample' but only shows the amplitude (with half-life factored in) at any given
-- time. This function cannot make use of the 'fdNoiseLevel' value of the 'FDComponent', because
-- this is a pure function that has no access to a random number generator.
fdComponentAmplitudeAt :: FDComponent -> Moment -> Sample
fdComponentAmplitudeAt fd t =
  (if fd ^. fdDecayRate <= 0.0 then id else
     flip (/) $ 1.0 + t / (fd ^. fdDecayRate)
  ) .
  (if fd ^. fdUndertone <= 0.0 || fd ^. fdUnderamp <= 0.0 then id else (*) $ (fd ^. fdUnderamp) *
    (1 + sin ((fd ^. fdUndertone) * minFrequency * pi * (t + (fd ^. fdUnderphase))) / 2)
  ) $
  (fd ^. fdAmplitude)

----------------------------------------------------------------------------------------------------

-- | A lazy functional data type isomorphic to 'FDSignal'.
data FDComponentList
  = FDComponentList
    { fdCompListLength :: !ComponentCount
    , fdCompListElems  :: [FDComponent]
    }

instance Show FDComponentList where
  show (FDComponentList{fdCompListLength=size,fdCompListElems=elems}) =
    "num elems: " ++ show size ++ '\n' : unlines (show <$> elems)

instance Semigroup FDComponentList where
  (<>) (FDComponentList{fdCompListLength=a,fdCompListElems=aElems})
       (FDComponentList{fdCompListLength=b,fdCompListElems=bElems})
    = FDComponentList{ fdCompListLength = a + b, fdCompListElems = aElems ++ bElems }

instance Monoid FDComponentList where
  mempty = FDComponentList{ fdCompListLength = 0, fdCompListElems = [] }
  mappend = (<>)

fdComponentList :: Int -> [FDComponent] -> FDComponentList
fdComponentList nelems comps = FDComponentList
  { fdCompListLength = nelems
  , fdCompListElems  = comps
  }

forEachFDComponent
  :: Monad m
  => FDComponentList
  -> (FDComponent -> m FDComponent)
  -> m FDComponentList
forEachFDComponent comps =
  liftM (\ c -> comps{ fdCompListElems = c }) . forM (fdCompListElems comps)

forEachFDComponent_
  :: Monad m
  => FDComponentList
  -> (FDComponent -> m ())
  -> m ()
forEachFDComponent_ = forM_ . fdCompListElems

--compMult :: [Frequency]
--compMult = componentMultipliers

--randFDComponents :: TFRand FDComponentList
--randFDComponents = do
--  (count, components) <- fmap (first sum . unzip . concat) $ forM compMult $ \ mul -> do
--    dice <- getRandom :: TFRand Word8
--    if dice > 4 then return [] else do
--      amp     <- onRandFloat $ (* (3/4) ) . (+ (1/3))
--      phase   <- onRandFloat id
--      ifUnder <- onRandFloat (\ i f -> if i < 0.2 then f else return 0.0)
--      decay   <- onRandFloat (* 2)
--      noise   <- onRandFloat (\ x -> if x <= 0.2 then x * 5.0 else 0.0)
--      under   <- ifUnder $ onRandFloat (* 7.5)
--      undph   <- ifUnder $ onRandFloat id
--      undamp  <- ifUnder $ onBeta5RandFloat (1 -)
--      return $ do
--        guard $ freq < nyquist
--        guard $ amp  > 0.1
--        return $ (,) 1 $ FDComponent
--          { theFDFreqCoef  = mul
--          , theFDAmplitude  = if mul > 1 then amp / mul else amp * mul
--          , theFDPhaseShift = phase
--          , theFDDecayRate  = decay
--          , theFDNoiseLevel = noise
--          , theFDUndertone  = under
--          , theFDUnderphase = undph
--          , theFDUnderamp   = undamp
--          }
--  return FDComponentList
--    { fdCompListLength = count + 1
--    , fdCompListElems  = FDComponent
--        { theFDFreqCoef  = base
--        , theFDAmplitude  = 1.0
--        , theFDPhaseShift = 0.0
--        , theFDDecayRate  = 0.0
--        , theFDNoiseLevel = 1.0
--        , theFDUndertone  = 0.0
--        , theFDUnderphase = 0.0
--        , theFDUnderamp   = 0.0
--        } : components
--    }

fdComponentInsert :: FDComponent -> FDComponentList -> FDComponentList
fdComponentInsert c list = FDComponentList
  { fdCompListElems  = c : fdCompListElems  list
  , fdCompListLength = 1 + fdCompListLength list
  }

----------------------------------------------------------------------------------------------------

-- | Frequency domain signal. This is a list of @('ProcGen.Frequency', 'ProcGen.Amplitude')@ pairs
-- used to define a signal in terms of a frequency domain graph.
data FDSignal
  = FDSignal
    { theFDBaseFreq       :: !NormFrequency
    , theFDMinFreq        :: !FreqCoeficient
    , theFDMaxFreq        :: !FreqCoeficient
    , theFDSize           :: !Int
      -- ^ 'theFDSignalVector' contains all elements expanded into a 1 dimensional array.  This
      -- parameter tells us how many actual components there are, which must be set by dividing the
      -- length of the array by the number of parameters in each 'FDComponent' vector there are.
    , theFDSignalVector   :: !(Unboxed.Vector ProcGenFloat)
    }
  deriving Eq

instance Show FDSignal where
  show fd = "size: "++show (fd ^. fdSize)++"\nminFreq: "++show (fd ^. fdMinFreq)++
    "\nmaxFreq: "++show (fd ^. fdMaxFreq)++"\nbaseFreq: "++show (fd ^. fdBaseFreq)++"\n"++
    unlines (listFDAssocs fd >>= \ (i, comp) -> [show i ++ ' ' : show comp])

instance BufferIDCT FDSignal where
  bufferIDCT mvec win fd =
    mapM_ (bufferFDComponentIDCT mvec win $ fd ^. fdBaseFreq) $ fdCompListElems $ listFDElems fd

fdMinFreq :: Lens' FDSignal FreqCoeficient
fdMinFreq = lens theFDMinFreq $ \ a b -> a{ theFDMinFreq = b }

fdMaxFreq :: Lens' FDSignal FreqCoeficient
fdMaxFreq = lens theFDMaxFreq $ \ a b -> a{ theFDMaxFreq = b }

fdBaseFreq :: Lens' FDSignal NormFrequency
fdBaseFreq = lens theFDBaseFreq $ \ a b -> a{ theFDBaseFreq = b }

fdSize :: Lens' FDSignal Int
fdSize = lens theFDSize $ \ a b -> a{ theFDSize = b }

fdSignalVector :: Lens' FDSignal (Unboxed.Vector ProcGenFloat)
fdSignalVector = lens theFDSignalVector $ \ a b -> a{ theFDSignalVector = b }

-- | Construct an empty 'FDSignal'.
emptyFDSignal :: FDSignal
emptyFDSignal = FDSignal
  { theFDBaseFreq     = NormFrequency 0
  , theFDMinFreq      = FreqCoeficient 0
  , theFDMaxFreq      = FreqCoeficient 0
  , theFDSize         = 0
  , theFDSignalVector = Unboxed.empty
  }

-- | Returns 'Prelude.True' if the 'FDSignal' contains no 'FDComponents'.
nullFDSignal :: FDSignal -> Bool
nullFDSignal = (<= 0) . theFDSize

--fdSignalComponents :: Iso' FDSignal FDComponentList
--fdSignalComponents = iso listFDElems fdSignal

-- | Create a new 'FDSignal'. Provide the number of components so that the amount of space to
-- allocate for the array does not need to be computed by counting components. Then provide a list
-- of components. The number of components is created regardless of the number of elements in the
-- list given, with zero values filling out space not covered by the list, or elements from the list
-- being dropped if there are mor than the given number of components.
fdSignal :: Frequency -> FDComponentList -> FDSignal
fdSignal freq fdcomps = case filter (not . nullFDComponent) (fdCompListElems fdcomps) of
    []       -> emptyFDSignal
    c0:elems -> let size = fdCompListLength fdcomps in runST $ execStateT
      (do let [freqi, ampi, phasi, decai, nois, undt, undph, undam, stepsize] = [0 .. 8]
          mvec <- lift $ Mutable.new $ size * stepsize
          let loop i = \ case
                []         -> return ()
                comp:elems -> seq i $! do
                  let wr off record = lift $ Mutable.write mvec (i + off) (record comp)
                  wr freqi (unwrapFreqCoef . theFDFreqCoef)
                  wr ampi  theFDAmplitude
                  wr phasi theFDPhaseShift
                  wr decai theFDDecayRate
                  wr nois  theFDNoiseLevel
                  wr undt  theFDUndertone
                  wr undph theFDUnderphase
                  wr undam theFDUnderamp
                  fdMinFreq %= min (comp ^. fdFreqCoef)
                  fdMaxFreq %= max (comp ^. fdFreqCoef)
                  loop (i + stepsize) elems
          loop 0 (c0 : elems)
          vec <- Unboxed.freeze mvec
          fdSignalVector .= vec
      )
      ( emptyFDSignal &~ do
          fdBaseFreq .= freqToNorm freq
          fdMinFreq  .= c0 ^. fdFreqCoef
          fdMaxFreq  .= c0 ^. fdFreqCoef
          fdSize     .= size
      )

-- | Extract a copy of every element triple from the 'FDSignal' as a list.
listFDElems :: FDSignal -> FDComponentList
listFDElems (FDSignal{theFDSignalVector=vec}) = FDComponentList
  { fdCompListElems  = loop $ Unboxed.toList vec
  , fdCompListLength = Unboxed.length vec `div` 8
  } where
      loop = \ case
        freq:amp:phase:decay:noise:undfrq:undphs:undamp:ax -> FDComponent
          { theFDFreqCoef   = FreqCoeficient freq
          , theFDAmplitude  = amp
          , theFDPhaseShift = phase
          , theFDDecayRate  = decay
          , theFDNoiseLevel = noise
          , theFDUndertone  = undfrq
          , theFDUnderphase = undphs
          , theFDUnderamp   = undamp
          } : loop ax
        _ -> []

-- | Similar to 'listFDElems', but includes the integer index associated with each element.
listFDAssocs :: FDSignal -> [(ComponentIndex, FDComponent)]
listFDAssocs = zip [0 ..] . fdCompListElems . listFDElems

-- | Extract a copy of a single element at a given index.
lookupFDComponent :: FDSignal -> ComponentIndex -> Maybe FDComponent
lookupFDComponent (FDSignal{theFDSignalVector=vec}) i =
  let f n = vec Unboxed.!? (i + n) in FDComponent
    <$> (FreqCoeficient <$> f 0) <*> f 1 <*> f 2 <*> f 3 <*> f 4 <*> f 5 <*> f 6 <*> f 7

-- | When generating a 'FDSignal' you need to generate components around a base frequency. This is a
-- list of recommended component frequencies multipliers. Each of these numbers is a rational
-- number. Simply multiply a these numbers times a base frequency to get your components.
componentMultipliers :: [Frequency]
componentMultipliers = do
  let f prime n = fmap (prime **) $ [negate n .. n]
  twos   <- f 2 5
  threes <- f 3 4
  fives  <- f 5 3
  sevens <- f 7 2
  [twos * threes * fives * sevens]

---- | Construct an 'ProcGen.Arbitrary.Arbitrary' 'FDSignal' with random 'ProcGen.Types.Frequency'
---- components generated with rational-numbered scaled frequency components around a given base
---- frequency. This will generate up to 1920 components, but is most likely to generate around 480
---- components.
--randFDSignal :: Frequency -> TFRand FDSignal
--randFDSignal = fmap fdSignal . randFDComponents

--randFDSignalIO :: Frequency -> IO FDSignal
--randFDSignalIO = seedIOEvalTFRand . randFDSignal

-- | Evaluates 'bufferIDCT' in the 'Control.Monad.ST.ST' monad producing a pure 'TDSignal' function.
pureIDCT :: TFGen -> Duration -> FDSignal -> TDSignal
pureIDCT gen dt fd = TDSignal
  { tdSamples = let n = durationSampleCount dt in Unboxed.create $ flip evalTFRandT gen $
      if n <= 0 then lift $ Mutable.new 0 else do
        mvec <- lift $ Mutable.new $ n + 1
        forM_ (fdCompListElems $ listFDElems fd) $
          bufferFDComponentIDCT mvec (TimeWindow{ timeStart = 0, timeEnd = dt }) (theFDBaseFreq fd)
        lift $ minMaxBuffer mvec >>= normalizeBuffer mvec
        return mvec
  }

-- | 'Frequency' multiplied times the 'FDComponent' 'fdFreqCoef' needs to be between 15.0 Hz and
-- the 'nyquist' frequency, otherwise this function evaluation returns @()@ without performing any
-- update on the given mutable vector.
bufferFDComponentIDCT
  :: PrimMonad m
  => Mutable.MVector (PrimState m) Sample
  -> TimeWindow Moment -> NormFrequency -> FDComponent -> TFRandT m ()
bufferFDComponentIDCT mvec win base fd =
  if 15.0 > freq || freq >= nyquist then return () else
  if fd ^. fdNoiseLevel <= 0
   then lift $ mapBuffer mvec (durationSampleCount <$> win) $ \ i e -> do
          let t = indexToTime i
          pure $ e + fdComponentSampleAt base fd t * fdComponentAmplitudeAt fd t
   else do
    -- A table of values is used here because we are not simply summing sine waves, we are creating
    -- a sine wave with a sigmoidal fade-in and fade-out, which requires two Float32
    -- multiplications, two calls to 'exp', two calls to 'recip', and one calls to 'sin' for each
    -- index. It is probably faster on most computer hardware to store these values to a table
    -- rather than compute them on the fly.
    if size > veclen then return () else do
      table <- lift (Mutable.new size)
      lift $ forM_ [0 .. size - 1] $ \ i -> Mutable.write table i
        $ sinePulse3 freq (0.0) (fd ^. fdPhaseShift)
        $ indexToTime i
      let loop  amp i j =
            if i >= veclen || j >= Mutable.length table then return () else do
              let decamp = (fd ^. fdAmplitude) * amp * fdComponentAmplitudeAt fd (indexToTime i)
              lift $ Mutable.write mvec i =<<
                liftM2 (+) (Mutable.read mvec i) ((* decamp) <$> Mutable.read table j)
              loop amp (i + 1) (j + 1)
      let pulses t = if t >= timeEnd win then return () else do
            amp <- getRandom
            let i = fst $ timeIndex t
            loop amp i 0
            pulses $! t + 2 / freq
      pulses 0.0
  where
    ti   = fst . timeIndex
    size = ti (3 / freq) + 1
    veclen = Mutable.length mvec
    freq = freqCoeficient base (fd ^. fdFreqCoef)

----------------------------------------------------------------------------------------------------

-- | Time domain signal.
newtype TDSignal = TDSignal { tdSamples :: Unboxed.Vector Sample }
  deriving Eq

instance TimeDomain TDSignal where
  sample td@(TDSignal vec) t =
    let (i, r) = timeIndex t
        a = vec Unboxed.! i
        b = vec Unboxed.! (i + 1)
    in  if i < tdSize td then a + (if i + 1 < tdSize td then (b - a) * r else 0.0) else 0.0

instance HasTimeWindow TDSignal SampleIndex where
  timeWindow = Just . tdTimeWindow

tdSize :: TDSignal -> Int
tdSize (TDSignal vec) = Unboxed.length vec

-- | Produces a list of all samples contained within the 'TDSignal' in order.
allTDSamples :: TDSignal -> (Int, [Sample])
allTDSamples (TDSignal vec) = (Unboxed.length vec, Unboxed.toList vec)

-- | Produce the time 'ProcGen.Types.Duration' value for the given 'TDSignal'.
tdTimeWindow :: TDSignal -> TimeWindow SampleIndex
tdTimeWindow (TDSignal vec) = TimeWindow{ timeStart = 0, timeEnd = Unboxed.length vec }

tdDuration :: TDSignal -> Duration
tdDuration = maybe 0 (twDuration . fmap indexToTime) . timeWindow

-- | Produce a lazy linked-list of all 'ProcGen.Types.Sample's stored in the 'TDSignal'. 
listTDSamples :: TDSignal -> TimeWindow Moment -> [Sample]
listTDSamples td@(TDSignal vec) =
  maybe [] (fmap (vec Unboxed.!) . twEnum) .
  (\ tw -> join $ twIntersect <$> timeWindow td <*> pure tw) .
  fmap (fst . timeIndex)

-- | Compute minimum and maximum values of the 'TDSignal'. 'TDSignals' that have been normalized
-- will almost always return values of @-1.0@ and @1.0@.
minMaxTDSignal :: TDSignal -> (Sample, Sample)
minMaxTDSignal td = minimum &&& maximum $ snd $ allTDSamples td

---- | Construct a random 'TDSignal' from a random 'FDSignal' constructed around a given base
---- 'ProcGen.Types.Frequency' by the 'randFDSignal' function.
--randTDSignalIO :: Duration -> Frequency -> IO (FDSignal, TDSignal)
--randTDSignalIO dt freq = do
--  gen <- initTFGen
--  (fd, gen) <- pure $ runTFRand (randFDSignal freq) gen
--  return (fd, pureIDCT gen dt fd)

-- | Create a RIFF-formatted WAV file at the given 'System.IO.FilePath' containing the 'TDSignal',
-- with 'ProcGen.Types.Sample' values rounded-off to 16-bit signed integer values
-- (little-endian). See "ProcGen.Music.WaveFile" for more information.
writeTDSignalFile :: FilePath -> TDSignal -> IO ()
writeTDSignalFile path = writeWave path . tdSamples

readTDSignalFile :: FilePath -> IO TDSignal
readTDSignalFile = fmap TDSignal . readWave

----------------------------------------------------------------------------------------------------

data FDView
  = FDView
    { theFDViewSignal   :: FDSignal
    , theFDViewAnimator :: AnimationControl
    }

instance Animated FDView where
  animationControl = lens theFDViewAnimator $ \ a b -> a{ theFDViewAnimator = b }

-- | Another constructor for 'FDView', but has a name consistent with the 'TDView' and 'tdView'
-- constructors.
fdView :: FDSignal -> FDView
fdView fd = FDView
  { theFDViewSignal   = fd
  , theFDViewAnimator = makeAnimationControl
  }

drawFDView :: FDView -> PixSize -> AnimationMoment -> CairoRender ()
drawFDView fdView (V2 w h) dt = do
  cairoRender $ do
    let fd     = theFDViewSignal fdView
    let base   = fd ^. fdBaseFreq
    let lo     = log (freqCoeficient base $ fd ^. fdMinFreq)
    let xscale = realToFrac w / (log (freqCoeficient base $ fd ^. fdMaxFreq) - lo)
    h <- pure $ realToFrac h
    cairoClearCanvas  1.0  1.0  1.0  0.8
    forEachFDComponent_ (listFDElems fd) $ \ fd@FDComponent{theFDFreqCoef=freq} -> do
      freq <- pure $ freqCoeficient base freq
      let x = realToFrac (round ((log freq - lo) * xscale) :: Int) + 0.5
      let y = realToFrac (1 - fdComponentAmplitudeAt fd (realToFrac dt)) * h + 0.5
      cairoSetColor (if fd ^. fdDecayRate == 0 then blue else red)
      Cairo.moveTo  x  (realToFrac h + 0.5)
      Cairo.lineTo  x  y
      Cairo.stroke
      Cairo.arc     x  y  1.5  0.0  (2.0 * pi)
      Cairo.fill
  screenPrinter $ do
    gridRow    .= 0
    gridColumn .= 0
    displayString (printf "time = %+.4f" (realToFrac (fdView ^. animFrame) :: ProcGenFloat))

animateFDView :: AnimationMoment -> GtkGUI FDView ()
animateFDView = realToFrac >>> \ dt -> do
  animFrame .= dt
  drawFDView <$> get <*> getWindowSize <*> pure dt >>= onCanvas

resizeFDView :: GtkGUI FDView ()
resizeFDView = drawFDView <$> getModel <*> getWindowSize <*> use animFrame >>= onCanvas

runFDView :: GtkGUI FDView ()
runFDView = do
  resizeEvents $ const resizeFDView
  keyboardEvents $ \ key -> do
    case key of
      Keyboard True mod key | noModifiers==mod -> case key of
        BackSpaceKey -> animFrame .= 0 >> animRun .= False
        CharKey ' '  -> animRun  .= True
        _            -> return ()
      _ -> return ()
    isNowAnimated <- use animRun
    stepFrameEvents $ if isNowAnimated then animateFDView else const disable
  resizeFDView

----------------------------------------------------------------------------------------------------

-- | When animating the 'TDView' graphically, the signal is drawn in real time, meaning every frame
-- of the animation draws __at least__ enough samples of the 'TDSignal' to cover the amount of time
-- the frame exists on screen. The number of samples drawn will usually be more than the amount of
-- time that elapsed, which means some samples toward the right of the graph will be re-drawn on the
-- next frame at the left of the graph.
-- 
-- The reason for this is because the graph of the 'TDSignal' is scaled such that the
-- 'tdViewInitTime' is an integer multiple of the 'tdViewBaseFreq', which ensures the base frequency
-- of the 'TDSignal' is always drawn such that the left-most edge of the GUI window aligns with the
-- start of the wave cycle (where the signal crosses the origin on it's rising phase). This
-- simulates the effect of the "trigger" feature found on most oscilloscopes.
data TDView
  = TDView
    { theTDViewAnimator    :: !AnimationControl
    , theTDViewSignal      :: !TDSignal
      -- ^ The signal to visualize.
    , theTDViewBaseFreq    :: !Frequency
      -- ^ The frequency at which a redraw is triggered.
    , theTDViewSampleCount :: !SampleCount
      -- ^ A signal sampled at 44100 Hz animated at 60 FPS means each animation frame can depict
      -- exactly 735 samples in real time. However if you prefer to have a 1-sample = 1-pixel
      -- visualization of the 'TDSignal' regardless of the actual GUI window size, set this value to
      -- the width of the GUI window whenever it is resized.
    , theTDViewFrameCount  :: !Int
      -- ^ This value does nothing for the visualization, it is updated by the animation event
      -- handler, and simply counts how many animation frames have elapsed.
    }

instance Animated TDView where
  animationControl = lens theTDViewAnimator $ \ a b -> a{ theTDViewAnimator = b }

-- | Constructs a new controller containing a 'TDView' from a 'TDSignal' and it's base
-- 'ProcGen.Types.Frequency'.
tdView :: TDSignal -> Frequency -> TDView
tdView td f = TDView
  { theTDViewAnimator    = makeAnimationControl
  , theTDViewSignal      = td
  , theTDViewBaseFreq    = f
  , theTDViewSampleCount = round $ sampleRate / animationRate
  , theTDViewFrameCount  = 0
  }

--tdViewFrameCount :: Lens' TDView Int
--tdViewFrameCount = lens theTDViewFrameCount $ \ a b -> a{ theTDViewFrameCount = b }

---- | Shift the 'animationCurrentFrame' so that the time is at least at the given
---- 'ProcGen.Types.Moment', and also shift forward a bit to make sure the 'animationCurrentFrame' is
---- on an integer number multiple of the 'tdViewBaseFreq'. Returns whether the 'ProcGen.Types.Moment'
---- given is beyond the end of the 'TDSignal'.
--tdViewAtTime :: Moment -> GtkGUI TDView ()
--tdViewAtTime t = do
--  v <- getModel
--  if t >= tdDuration (theTDViewSignal v)
--   then (animRun .= False) >> disable
--   else do
--     tdViewFrameCount %= (+ 1)
--     animFrame .= realToFrac
--       (realToFrac (ceiling (t * theTDViewBaseFreq v) :: Int) / theTDViewBaseFreq v)

drawTDView :: TDView -> PixSize -> CairoRender ()
drawTDView v (V2 (SampCoord w) (SampCoord h)) = do
  cairoRender $ do
    let count  = max 1 $ theTDViewSampleCount v
    let origin = realToFrac h / 2
    let pixTime = realToFrac w / realToFrac count :: Double
      -- the ^ amount of time a single pixel on screen represents
    let f t = origin - origin * realToFrac (sample (theTDViewSignal v) t)
    cairoClearCanvas     1.0  1.0  1.0  0.8
    Cairo.setSourceRGBA  0.0  0.0  0.0  1.0
    Cairo.moveTo  0.0  origin
    Cairo.lineTo (realToFrac w) origin
    Cairo.stroke
    Cairo.setSourceRGBA  0.0  0.0  1.0  1.0
    Cairo.moveTo  0.0 $ f $ realToFrac (v ^. animFrame)
    forM_ (realToFrac <$> [1 .. count - 1]) $ \ pix -> do
      let x = pixTime * pix
      let t = realToFrac x / sampleRate + (realToFrac $ v ^. animFrame)
      let y = f t
      Cairo.lineTo x y
    Cairo.stroke
  screenPrinter $ do
    gridRow    .= 0
    gridColumn .= 0
    displayString (printf "time = %+.4f" (realToFrac (v ^. animFrame) :: ProcGenFloat))

resizeTDView :: GtkGUI TDView ()
resizeTDView = drawTDView <$> getModel <*> getWindowSize >>= onCanvas

animateTDView :: AnimationMoment -> GtkGUI TDView ()
animateTDView = realToFrac >>> \ dt -> do
  beyond <- gets $ (>=) dt . tdDuration . theTDViewSignal
  if beyond then stepFrameEvents $ const disable else do
    animFrame .= realToFrac dt
    drawTDView <$> getModel <*> getWindowSize >>= onCanvas

clickMouseTDView :: Mouse -> GtkGUI TDView ()
clickMouseTDView (Mouse _ pressed _mod button _loc) = when pressed $ case button of
  RightClick -> do
    animFrame .= 0
    animRun   .= False
    drawTDView <$> getModel <*> getWindowSize >>= onCanvas
  _          -> do
    animRun %= not -- This couldn't possibly toggle the animate bit.... NOT!!!
    isNowAnimated <- use animRun
    stepFrameEvents $ if isNowAnimated then animateTDView else const disable

-- | Use this to attach a @('Happlets.GUI.Happlet' 'TDView')@ to a window.
runTDView :: GtkGUI TDView ()
runTDView = do
  mouseEvents MouseButton clickMouseTDView
  keyboardEvents $ \ key -> do
    case key of
      Keyboard True mod key | noModifiers==mod -> case key of
        BackSpaceKey -> animFrame .= 0 >> animRun .= False
        CharKey ' '  -> animRun  .= True
        _            -> return ()
      _ -> return ()
    isNowAnimated <- use animRun
    unless isNowAnimated $ stepFrameEvents $ const disable
  resizeEvents $ const resizeTDView
  resizeTDView

----------------------------------------------------------------------------------------------------

-- | A GUI designed to allow you to draw a random frequency domain graph with the mouse cursor. As
-- you draw, random 'FDComponents' are added or removed (depending on whether you have set
-- 'InsertMode' or 'RemoveMode'). You can then scale-up these components to an 'FDSignal' which can
-- then be converted to a 'TDSignal'.
data CreateRandomFD
  = CreateRandomFD
    { theDrawingMode   :: DrawingMode
    , theCreatedVector :: Mutable.IOVector ProcGenFloat
    }

data DrawingMode = InsertMode | RemoveMode deriving (Eq, Ord, Show, Read)


