-- | This module defines the data types and functions for creating sound effects that can be used as
-- musical instruments.
module ProcGen.Music.Synth
  ( -- * A language for defining musical sounds
    Synth(..), SynthState(..), SynthElement(..), FDSignalShape(..),
    initSynth, runSynth, synthElements, synthBuffer, synthTFGen, synthFrequency,
    resizeSynthBuffer, resetSynthBuffer,
    fractions,
    -- * Buffering
    BufferIDCT(..), newSampleBuffer,
    -- * Elements of frequency domain functions
    FDComponent(..), emptyFDComponent,
    FDComponentList, randFDComponents, fdComponentInsert,
    fdComponentSampleAt, fdComponentAmplitudeAt,
    -- * Signals as frequency domain functions
    FDSignal, fdSignalVector, emptyFDSignal, nullFDSignal,
    fdSignal, fdSize, fdMinFreq, fdMaxFreq, fdBaseFreq,
    listFDElems, listFDAssocs, lookupFDComponent,
    componentMultipliers, randFDSignal, randFDSignalIO,
    -- * Time Domain Function Construction
    TDSignal, allTDSamples, listTDSamples, tdTimeWindow, tdDuration, pureIDCT,
    minMaxTDSignal, randTDSignalIO, writeTDSignalFile, readTDSignalFile,
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
import           ProcGen.Collapsible
import           ProcGen.Music.WaveFile
import           ProcGen.Properties
import           ProcGen.VectorBuilder

import           Control.Arrow
import           Control.Monad.ST

import           Data.Semigroup
import qualified Data.Text                   as Strict
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable
import           Data.Word

import           Linear.V2

import qualified Graphics.Rendering.Cairo    as Cairo

import           Text.Printf

----------------------------------------------------------------------------------------------------

newtype Synth a = Synth { unwrapSynth :: StateT SynthState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState SynthState Synth where { state = Synth . state; }
instance Semigroup a => Semigroup (Synth a) where { a <> b = (<>) <$> a <*> b; }
instance Monoid a => Monoid (Synth a) where
  mappend a b = mappend <$> a <*> b
  mempty = pure mempty

data SynthState
  = SynthState
    { theSynthElements  :: [SynthElement]
      -- ^ Elements to be used to render a 'TDSignal'.
    , theSynthTFGen     :: TFGen
    , theSynthBuffer    :: Mutable.IOVector Sample
    , theSynthFrequency :: Frequency
    }

-- | An element is a frozen vector of 'FDComponents' with a name and a few other properties that can
-- be used to display it in a GUI.
data SynthElement
  = SynthElement
    { theSynthElemName   :: Strict.Text
    , theSynthElemColor  :: Color
    , theSynthElemSignal :: FDSignal
    }

instance BufferIDCT SynthElement where
  bufferIDCT mvec win = bufferIDCT mvec win . theSynthElemSignal

instance BufferIDCT SynthState where
  bufferIDCT mvec win = mapM_ (bufferIDCT mvec win) . theSynthElements

-- | The elements we are working with.
synthElements :: Lens' SynthState [SynthElement]
synthElements = lens theSynthElements $ \ a b -> a{ theSynthElements = b }

synthTFGen :: Lens' SynthState TFGen
synthTFGen = lens theSynthTFGen $ \ a b -> a{ theSynthTFGen = b }

synthFrequency :: Lens' SynthState Frequency
synthFrequency = lens theSynthFrequency $ \ a b -> a{ theSynthFrequency = b }

synthBuffer :: Lens' SynthState (Mutable.IOVector Sample)
synthBuffer = lens theSynthBuffer $ \ a b -> a{ theSynthBuffer = b }

initSynth :: IO SynthState
initSynth = SynthState [] <$> initTFGen <*> newSampleBuffer 4.0 <*> pure 256.0

runSynth :: Synth a -> SynthState -> IO (a, SynthState)
runSynth (Synth f) = runStateT f

-- | Select the Nth prime number from the 'ProcGen.PrimeNumbers.all16BitPrimes' table.
primeFunc :: Int -> ProcGenFloat
primeFunc = realToFrac . (all16BitPrimes Unboxed.!)

-- | This function generates a list of prime fractions, where the numerator and denominator are both
-- prime numbers, from a selection of primes passed as parameters to this function. To select a
-- prime number, simply pass an 'Prelude.Int' and it will be selected from a table of primes, where
-- the integer @0@ selects the first prime number @2@, the integer @1@ selects the next prime number
-- 3 and so on. Pass two selections of primes, one for the numerators of the generated fractions,
-- and one for the denominators. Pass a filter function to filter out numerator/denominator pairs to
-- limit the selection of primes, for example passing @(Prelude.<)@ will select all pairs where the
-- numerator is less than the denominator, therefore the list of generated fractions will all be
-- less than 1 and greater than zero.
fractions :: (Int -> Int -> Bool) -> [Int] -> [Int] -> [ProcGenFloat]
fractions filt nums denoms =
  [primeFunc num / primeFunc denom | denom <- denoms, num <- nums, num /= denom, filt num denom]

-- | Delete the current buffer and replace it with a new one of the given size.
resetSynthBuffer :: Duration -> Synth ()
resetSynthBuffer = liftIO . newSampleBuffer >=> assign synthBuffer

-- | Resize the current buffer, copy the signal within to the resized buffer.
resizeSynthBuffer :: TimeWindow Moment -> Synth ()
resizeSynthBuffer tw = do
  oldbuf <- gets theSynthBuffer
  let iw     = durationSampleCount <$> tw
  let newlen = timeEnd iw - timeStart iw + 1
  case compare newlen $ Mutable.length oldbuf of
    EQ -> return ()
    GT -> liftIO (Mutable.grow oldbuf newlen) >>= assign synthBuffer
    LT -> liftIO (Mutable.clone $ Mutable.slice (timeStart iw) (timeEnd iw) oldbuf) >>=
      assign synthBuffer

----------------------------------------------------------------------------------------------------

-- | A data type for declaring the shape of an 'FDSignal'.
data FDSignalShape
  = FDSignalShape
    { theFDShapeBase       :: Frequency
      -- ^ The base frequency, which splits the audible spectrum into "lower" and "upper" bands.
    , theFDShapeLowerLimit :: Frequency
      -- ^ The lowest audible frequency of the shape
    , theFDShapeLoEnvelope :: Envelope
      -- ^ The envelope that defines the shape of the 'FDSignal' to be produced by this
      -- structure. The 'ProcGen.Types.Envelope' function is expected to be defined only on the
      -- range between and including 0.0 and 1.0, and will be transformed to fit in the region of
      -- frequencies between 'theFDShapeLowerLimit' and 'theFDShapeBase'.
    , theFDShapeUpperLimit :: Frequency
      -- ^ Like 'theFDShapeLowerLimit' but defines a point above 'theFDShapeBase' frequency.
    , theFDShapeHiEnvelope :: Envelope
      -- ^ Like 'theFDShapeLoEnvelope' but defines an envelope applied to the region of frequencies
      -- between 'theFDShapeBase' and 'theFDShapeUpperLimit'.
    }



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

data FDComponent
  = FDComponent
    { fdFrequency  :: !Frequency  -- ^ must be between 15.0 and 22100.0 Hz
    , fdAmplitude  :: !Amplitude 
    , fdPhaseShift :: !PhaseShift -- ^ must be between 0 and 1, automatically scaled to 2*pi
    , fdDecayRate  :: !HalfLife   -- ^ set to zero for no decay
    , fdNoiseLevel :: !Amplitude
      -- ^ how much noise to apply, as a measure of variance in the 'fdAmplitude'. A value of @1.0@
      -- means the amplitude of each cycle varies is multiplied by a random number between @0.0@ to
      -- @1.0@. A value of @0.5@ means the amplitude of each cycle is mulitplied by a random number
      -- between @0.5@ (half volume) and @1.0@ (full volume).
    , fdUndertone  :: !Frequency
      -- ^ set whehther the amplitude is also oscillating, must be between 0.0 and 7.5 Hz, zero
      -- indicates no oscillation. Also if the 'fdDecayRate' has a half life less than the period of
      -- the 'fdUndertone', the 'fdUndertone' is ignored.
    , fdUnderphase :: !PhaseShift
      -- ^ the phase shift for the 'fdUndertone'
    , fdUnderamp   :: !Amplitude
      -- ^ the amplitude of the 'fdUndertone'
    }
  deriving (Eq, Ord)

instance Show FDComponent where
  show fd = printf
    "(FD freq=%+.4f amp=%+.4f phase=%+.4f decay=%+.4f noise=%+.4f undrtone=%+.4f undrphse=%.4f)"
    (fdFrequency fd) (fdAmplitude fd) (fdPhaseShift fd) (fdDecayRate fd)
    (fdNoiseLevel fd) (fdUndertone fd) (fdUnderphase fd)

instance Collapsible Float FDComponent where
  collapse =
    buildRecord fdFrequency  <>
    buildRecord fdAmplitude  <>
    buildRecord fdPhaseShift <>
    buildRecord fdDecayRate  <>
    buildRecord fdNoiseLevel <>
    buildRecord fdUndertone  <>
    buildRecord fdUnderphase <>
    buildRecord fdUnderamp
  uncollapse = error "TODO: (uncollapse :: UVec.Vector -> FDComponent)"

emptyFDComponent :: FDComponent
emptyFDComponent = FDComponent
  { fdFrequency  = 0
  , fdAmplitude  = 0
  , fdPhaseShift = 0
  , fdDecayRate  = 0
  , fdNoiseLevel = 0
  , fdUndertone  = 0
  , fdUnderphase = 0
  , fdUnderamp   = 0
  }

-- | Returns 'Prelude.True' if either th frequency or amplitude are zero.
nullFDComponent :: FDComponent -> Bool
nullFDComponent fd = fdFrequency fd == 0 || fdAmplitude fd == 0

-- | Computes the exact 'ProcGen.Types.Sample' value at a given time produced by this
-- component. This function cannot make use of the 'fdNoiseLevel' value of the 'FDComponent',
-- because this is a pure function that has no access to a random number generator.
fdComponentSampleAt :: FDComponent -> Moment -> Sample
fdComponentSampleAt fd t = if fdFrequency fd > nyquist then 0 else
  fdComponentAmplitudeAt fd t * sin (fdPhaseShift fd + 2.0 * pi * fdFrequency fd * t)

-- | Like 'fdComponentSample' but only shows the amplitude (with half-life factored in) at any given
-- time. This function cannot make use of the 'fdNoiseLevel' value of the 'FDComponent', because
-- this is a pure function that has no access to a random number generator.
fdComponentAmplitudeAt :: FDComponent -> Moment -> Sample
fdComponentAmplitudeAt fd t =
  (if fdDecayRate fd <= 0.0 then id else
     flip (/) $ 1.0 + t / fdDecayRate fd
  ) .
  (if fdUndertone fd <= 0.0 || fdUnderphase fd <= 0.0 || fdUnderamp fd <= 0.0 then id else
     (*) $ fdUnderamp fd * (1 + sin (fdUndertone fd * 2 * pi * t + fdUnderphase fd) / 2)
  ) $
  (fdAmplitude fd)

randPhase :: MonadRandom m => m PhaseShift
randPhase = onRandFloat $ (* pi) . subtract 1 . (* 2)

----------------------------------------------------------------------------------------------------

-- | A lazy functional data type isomorphic to 'FDSignal'.
data FDComponentList
  = FDComponentList
    { theFDCompListLength :: !ComponentCount
    , theFDCompListElems  :: [FDComponent]
    }

instance Show FDComponentList where
  show (FDComponentList{theFDCompListLength=size,theFDCompListElems=elems}) =
    "num elems: " ++ show size ++ '\n' : unlines (show <$> elems)

instance Semigroup FDComponentList where
  (<>) (FDComponentList{theFDCompListLength=a,theFDCompListElems=aElems})
       (FDComponentList{theFDCompListLength=b,theFDCompListElems=bElems})
    = FDComponentList{ theFDCompListLength = a + b, theFDCompListElems = aElems ++ bElems }

instance Monoid FDComponentList where
  mempty = FDComponentList{ theFDCompListLength = 0, theFDCompListElems = [] }
  mappend = (<>)

randFDComponents :: Frequency -> TFRand FDComponentList
randFDComponents base = do
  (count, components) <- fmap (first sum . unzip . concat) $ forM compMult $ \ mul -> do
    dice <- getRandom :: TFRand Word8
    if dice > 4 then return [] else do
      amp     <- onRandFloat $ (* (3/4) ) . (+ (1/3))
      phase   <- randPhase
      ifUnder <- onRandFloat (\ i f -> if i < 0.2 then f else return 0.0)
      decay   <- onRandFloat (* 2)
      noise   <- onRandFloat (\ x -> if x <= 0.2 then x * 5.0 else 0.0)
      under   <- ifUnder $ onRandFloat (* 7.5)
      undph   <- ifUnder $ randPhase
      undamp  <- ifUnder $ onBeta5RandFloat (1 -)
      return $ do
        let freq = base * mul
        guard $ freq < nyquist
        guard $ amp  > 0.1
        return $ (,) 1 $ FDComponent
          { fdFrequency  = base * mul
          , fdAmplitude  = if mul > 1 then amp / mul else amp * mul
          , fdPhaseShift = phase
          , fdDecayRate  = decay
          , fdNoiseLevel = noise
          , fdUndertone  = under
          , fdUnderphase = undph
          , fdUnderamp   = undamp
          }
  return FDComponentList
    { theFDCompListLength = count + 1
    , theFDCompListElems  = FDComponent
        { fdFrequency  = base
        , fdAmplitude  = 1.0
        , fdPhaseShift = 0.0
        , fdDecayRate  = 0.0
        , fdNoiseLevel = 1.0
        , fdUndertone  = 0.0
        , fdUnderphase = 0.0
        , fdUnderamp   = 0.0
        } : components
    }

fdComponentInsert :: FDComponent -> FDComponentList -> FDComponentList
fdComponentInsert c list = FDComponentList
  { theFDCompListElems  = c : theFDCompListElems  list
  , theFDCompListLength = 1 + theFDCompListLength list
  }

----------------------------------------------------------------------------------------------------

-- | Frequency domain signal. This is a list of @('ProcGen.Frequency', 'ProcGen.Amplitude')@ pairs
-- used to define a signal in terms of a frequency domain graph.
data FDSignal
  = FDSignal
    { fdMinFreq      :: !Frequency
    , fdMaxFreq      :: !Frequency
    , fdBaseFreq     :: !Frequency
    , fdSize         :: !Int
    , fdSignalVector :: !(Unboxed.Vector ProcGenFloat)
    }
  deriving Eq

instance Show FDSignal where
  show fd = "size: "++show (fdSize fd)++"\nminFreq: "++show (fdMinFreq fd)++
    "\nmaxFreq: "++show (fdMaxFreq fd)++"\nbaseFreq: "++show (fdBaseFreq fd)++"\n"++
    unlines (listFDAssocs fd >>= \ (i, comp) -> [show i ++ ' ' : show comp])

instance BufferIDCT FDSignal where
  bufferIDCT mvec win = let lim f = 15.0 <= f && f <= nyquist in
    mapM_ (bufferIDCT mvec win) . filter (lim . fdFrequency) . listFDElems

-- | Construct an empty 'FDSignal'.
emptyFDSignal :: FDSignal
emptyFDSignal = FDSignal
  { fdMinFreq      = 0
  , fdMaxFreq      = 0
  , fdBaseFreq     = 0
  , fdSize         = 0
  , fdSignalVector = Unboxed.empty
  }

-- | Returns 'Prelude.True' if the 'FDSignal' contains no 'FDComponents'.
nullFDSignal :: FDSignal -> Bool
nullFDSignal = (<= 0) . fdSize

-- | Create a new 'FDSignal'. Provide the number of components so that the amount of space to
-- allocate for the array does not need to be computed by counting components. Then provide a list
-- of components. The number of components is created regardless of the number of elements in the
-- list given, with zero values filling out space not covered by the list, or elements from the list
-- being dropped if there are mor than the given number of components.
fdSignal :: FDComponentList -> FDSignal
fdSignal fdcomps = case filter (not . nullFDComponent) (theFDCompListElems fdcomps) of
    []       -> emptyFDSignal
    c0:elems -> let size = theFDCompListLength fdcomps in runST $ execStateT
      (do let [freqi, ampi, phasi, decai, stepsize] = [0 .. 4]
          mvec <- lift $ Mutable.new $ size * stepsize
          let loop i = \ case
                []         -> return ()
                comp:elems -> seq i $! do
                  let wr off record = lift $ Mutable.write mvec (i + off) (record comp)
                  wr freqi fdFrequency
                  wr ampi  fdAmplitude
                  wr phasi fdPhaseShift
                  wr decai fdDecayRate
                  modify $ \ st -> st
                    { fdMinFreq = min (fdMinFreq st) (fdFrequency comp)
                    , fdMaxFreq = max (fdMaxFreq st) (fdFrequency comp)
                    }
                  loop (i + 4) elems
          loop 0 (c0 : elems)
          vec <- Unboxed.freeze mvec
          modify $ \ st -> st{ fdSignalVector = vec }
      ) emptyFDSignal
        { fdBaseFreq = fdFrequency c0
        , fdMinFreq  = fdFrequency c0
        , fdMaxFreq  = fdFrequency c0
        , fdSize     = size
        }

-- | Extract a copy of every element triple from the 'FDSignal' as a list.
listFDElems :: FDSignal -> [FDComponent]
listFDElems (FDSignal{fdSignalVector=vec}) = loop $ Unboxed.toList vec where
  loop = \ case
    freq:amp:phase:decay:noise:undfrq:undphs:undamp:ax -> FDComponent
      { fdFrequency  = freq
      , fdAmplitude  = amp
      , fdPhaseShift = phase
      , fdDecayRate  = decay
      , fdNoiseLevel = noise
      , fdUndertone  = undfrq
      , fdUnderphase = undphs
      , fdUnderamp   = undamp
      } : loop ax
    _ -> []

-- | Similar to 'listFDElems', but includes the integer index associated with each element.
listFDAssocs :: FDSignal -> [(ComponentIndex, FDComponent)]
listFDAssocs = zip [0 ..] . listFDElems

-- | Extract a copy of a single element at a given index.
lookupFDComponent :: FDSignal -> ComponentIndex -> Maybe FDComponent
lookupFDComponent (FDSignal{fdSignalVector=vec}) i =
  let f n = vec Unboxed.!? (i + n)
  in  FDComponent <$> f 0 <*> f 1 <*> f 2 <*> f 3 <*> f 4 <*> f 5 <*> f 6 <*> f 7

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

compMult :: [Frequency]
compMult = componentMultipliers

-- | Construct an 'ProcGen.Arbitrary.Arbitrary' 'FDSignal' with random 'ProcGen.Types.Frequency'
-- components generated with rational-numbered scaled frequency components around a given base
-- frequency. This will generate up to 1920 components, but is most likely to generate around 480
-- components.
randFDSignal :: Frequency -> TFRand FDSignal
randFDSignal = fmap fdSignal . randFDComponents

randFDSignalIO :: Frequency -> IO FDSignal
randFDSignalIO = seedIOEvalTFRand . randFDSignal

-- | Evaluates 'bufferIDCT' in the 'Control.Monad.ST.ST' monad producing a pure 'TDSignal' function.
pureIDCT :: TFGen -> Duration -> FDSignal -> TDSignal
pureIDCT gen dt fd = TDSignal
  { tdSamples = let n = durationSampleCount dt in Unboxed.create $ flip evalTFRandT gen $
      if n <= 0 then lift $ Mutable.new 0 else do
        mvec <- lift $ Mutable.new $ n + 1
        bufferIDCT mvec (TimeWindow{ timeStart = 0, timeEnd = dt }) fd
        lift $ minMaxBuffer mvec >>= normalizeBuffer mvec
        return mvec
  }

instance BufferIDCT FDComponent where
  bufferIDCT mvec win fd = if fdNoiseLevel fd <= 0 then bufferIDCT mvec win fd else do
    let ti = fst . timeIndex
    let size = ti (3 / fdFrequency fd) + 1
    -- A table of values is used here because we are not simply summing sine waves, we are creating
    -- a sine wave with a sigmoidal fade-in and fade-out, which requires two Float32
    -- multiplications, two calls to 'exp', two calls to 'recip', and one calls to 'sin' for each
    -- index. It is probably faster on most computer hardware to store these values to a table
    -- rather than compute them on the fly.
    if size > Mutable.length mvec then return () else do
      table <- lift (Mutable.new size)
      lift $ forM_ [0 .. size - 1] $ \ i ->
        Mutable.write table i $ sinePulse3 (fdFrequency fd) (0.0) (fdPhaseShift fd) $ indexToTime i
      let loop  amp i j =
            if i >= Mutable.length mvec || j >= Mutable.length table then return () else do
              let decamp = fdAmplitude fd * amp * fdComponentAmplitudeAt fd (indexToTime i)
              lift $ Mutable.write mvec i =<<
                liftM2 (+) (Mutable.read mvec i) ((* decamp) <$> Mutable.read table j)
              loop amp (i + 1) (j + 1)
      let pulses t = if t >= timeEnd win then return () else do
            amp <- getRandom
            let i = fst $ timeIndex t
            loop amp i 0
            pulses $! t + 2 / fdFrequency fd
      pulses 0.0

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
  timeWindow = tdTimeWindow

tdSize :: TDSignal -> Int
tdSize (TDSignal vec) = Unboxed.length vec

-- | Produces a list of all samples contained within the 'TDSignal' in order.
allTDSamples :: TDSignal -> (Int, [Sample])
allTDSamples (TDSignal vec) = (Unboxed.length vec, Unboxed.toList vec)

-- | Produce the time 'ProcGen.Types.Duration' value for the given 'TDSignal'.
tdTimeWindow :: TDSignal -> TimeWindow SampleIndex
tdTimeWindow (TDSignal vec) = TimeWindow{ timeStart = 0, timeEnd = Unboxed.length vec }

tdDuration :: TDSignal -> Duration
tdDuration = twDuration . fmap indexToTime . timeWindow

-- | Produce a lazy linked-list of all 'ProcGen.Types.Sample's stored in the 'TDSignal'. 
listTDSamples :: TDSignal -> TimeWindow Moment -> [Sample]
listTDSamples td@(TDSignal vec) =
  maybe [] (fmap (vec Unboxed.!) . twEnum) . twIntersect (timeWindow td) . fmap (fst . timeIndex)

-- | Compute minimum and maximum values of the 'TDSignal'. 'TDSignals' that have been normalized
-- will almost always return values of @-1.0@ and @1.0@.
minMaxTDSignal :: TDSignal -> (Sample, Sample)
minMaxTDSignal td = minimum &&& maximum $ snd $ allTDSamples td

-- | Construct a random 'TDSignal' from a random 'FDSignal' constructed around a given base
-- 'ProcGen.Types.Frequency' by the 'randFDSignal' function.
randTDSignalIO :: Duration -> Frequency -> IO (FDSignal, TDSignal)
randTDSignalIO dt freq = do
  gen <- initTFGen
  (fd, gen) <- pure $ runTFRand (randFDSignal freq) gen
  return (fd, pureIDCT gen dt fd)

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
    let lo     = log (fdMinFreq fd)
    let xscale = realToFrac w / (log (fdMaxFreq fd) - lo)
    h <- pure $ realToFrac h
    cairoClearCanvas  1.0  1.0  1.0  0.8
    forM_ (listFDElems fd) $ \ fd@FDComponent{fdFrequency=freq} -> do
      let x = realToFrac (round ((log freq - lo) * xscale) :: Int) + 0.5
      let y = realToFrac (1 - fdComponentAmplitudeAt fd (realToFrac dt)) * h + 0.5
      cairoSetColor (if fdDecayRate fd == 0 then blue else red)
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


