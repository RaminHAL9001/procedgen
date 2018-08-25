-- | This module defines the data types and functions for creating sound effects that can be used as
-- musical instruments.
module ProcGen.Music.Synth
  ( -- * A language for defining musical sounds
    Synth(..), SynthState(..), SynthElement(..), runSynth,
    -- * Signals as frequency domain functions
    FDComponent(..), emptyFDComponent,
    FDComponentList, randFDComponents, fdComponentInsert,
    FDSignal, fdSignalVector, emptyFDSignal, nullFDSignal,
    fdSignal, fdSize, fdMinFreq, fdMaxFreq, fdBaseFreq,
    listFDElems, listFDAssocs, lookupFDComponent,
    componentMultipliers, randFDSignal, randFDSignalIO,
    randAmpPulse, randAmpPulsePure,
    -- * Time Domain Function Construction
    TDSignal, allTDSamples, listTDSamples, tdTimeWindow, tdDuration,
    pureIDCT, bufferIDCT, idctRandAmpPulsePure, idctRandAmpPulse,
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
--import qualified Data.Vector                 as Boxed
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

newtype SynthState = SynthState { workingFDSignals :: [SynthElement]}

-- | An element is a frozen vector of 'FDComponents' with a name and a few other properties that can
-- be used to display it in a GUI.
data SynthElement
  = SynthElement
    { theSynthElemName   :: Strict.Text
    , theSynthElemColor  :: Color
    , theSynthElemSignal :: FDSignal
    }

runSynth :: Synth a -> SynthState -> IO (a, SynthState)
runSynth (Synth f) = runStateT f

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
    buildRecord fdUnderphase
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
  }

-- | Returns 'Prelude.True' if either th frequency or amplitude are zero.
nullFDComponent :: FDComponent -> Bool
nullFDComponent fd = fdFrequency fd == 0 || fdAmplitude fd == 0

-- | Computes the exact 'ProcGen.Types.Sample' value at a given time produced by this component.
fdComponentSampleAt :: FDComponent -> Moment -> Sample
fdComponentSampleAt fd t =
  let (FDComponent{fdFrequency=freq,fdPhaseShift=phase}) = fd
  in  if freq > nyquist then 0 else
        fdComponentAmplitudeAt fd t * sin (phase + 2.0 * pi * freq * t)

-- | Like 'fdComponentSample' but only shows the amplitude (with half-life factored in) at any given
-- time.
fdComponentAmplitudeAt :: FDComponent -> Moment -> Sample
fdComponentAmplitudeAt fd t = 
  let thl = if fdDecayRate fd <= 0.0 then 1.0 else t / fdDecayRate fd + 1.0
  in  fdAmplitude fd / thl

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
      amp   <- onRandFloat $ (* (3/4) ) . (+ (1/3))
      phase <- randPhase
      decay <- onRandFloat (* 2)
      noise <- onRandFloat (\ x -> if x <= 0.2 then x * 5.0 else 0.0)
      under <- onRandFloat (* 7.5)
      undph <- randPhase
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
    freq:amp:phase:decay:noise:undfrq:undphs:ax -> FDComponent
      { fdFrequency  = freq
      , fdAmplitude  = amp
      , fdPhaseShift = phase
      , fdDecayRate  = decay
      , fdNoiseLevel = noise
      , fdUndertone  = undfrq
      , fdUnderphase = undphs
      } : loop ax
    _ -> []

-- | Similar to 'listFDElems', but includes the integer index associated with each element.
listFDAssocs :: FDSignal -> [(ComponentIndex, FDComponent)]
listFDAssocs = zip [0 ..] . listFDElems

-- | Extract a copy of a single element at a given index.
lookupFDComponent :: FDSignal -> ComponentIndex -> Maybe FDComponent
lookupFDComponent (FDSignal{fdSignalVector=vec}) = (* 7) >>> \ i -> FDComponent
  <$> (vec Unboxed.!? (i + 0))
  <*> (vec Unboxed.!? (i + 1))
  <*> (vec Unboxed.!? (i + 2))
  <*> (vec Unboxed.!? (i + 3))
  <*> (vec Unboxed.!? (i + 4))
  <*> (vec Unboxed.!? (i + 5))
  <*> (vec Unboxed.!? (i + 6))

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

-- | This function creates a 'TDSignal' by performing the __I__nverse __D__iscrete __C__osine
-- __T__ransform on the given 'FDSignal'.
pureIDCT :: Duration -> FDSignal -> TDSignal
pureIDCT dt fd = TDSignal
  { tdSamples = let n = durationSampleCount dt in Unboxed.create $
      if n <= 0 then Mutable.new 0 else do
        mvec <- Mutable.new $ n + 1
        bufferIDCT mvec (TimeWindow{ timeStart = 0, timeEnd = dt }) fd
        minMaxVec mvec >>= normalize mvec
        return mvec
  }

-- | Perform the 'idct' function on a mutable buffer using either of the stateful monadic function
-- types @IO@ or @'Control.Monad.ST.ST'@, that way numerous 'idct' operations can be use
-- cumulatively on 'Mutable.STVector's. WARNING: the results are not normalized, so you will almost
-- certainly end up with elements in the vector that are well above 1 or well below -1. Evaluate @\
-- mvec -> 'minMaxVec' mvec >>= normalize 'mvec'@ before freezing the 'Mutable.STVector'.
bufferIDCT
  :: PrimMonad m
  => Mutable.MVector (PrimState m) Sample -> TimeWindow Moment -> FDSignal -> m ()
bufferIDCT mvec win fd =
  forM_ (twIndicies (Mutable.length mvec) win) $ \ i -> Mutable.write mvec i $! sum $ do
    fd <- listFDElems fd
    guard $ fdFrequency fd <= nyquist
    [fdComponentSampleAt fd $ indexToTime i]

-- | Accumulates a signal into an 'Mutable.STVector' that consists of a single sine wave, but the
-- amplitude of the wave is randomly modified for every cycle. This creates noise with a very narrow
-- frequency spectrum.
randAmpPulse
  :: forall m . PrimMonad m
  => Mutable.MVector (PrimState m) Sample
  -> TimeWindow Moment -> FDComponent -> TFRandT m ()
randAmpPulse mvec win fd = do
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
          amp <- getRandom :: TFRandT m Float
          let i = fst $ timeIndex t
          loop amp i 0
          pulses $! t + 2 / fdFrequency fd
    pulses 0.0

-- | Renders a 'FDSignal' using 'randAmpPulseST', rather than a clean sine wave. This can be used to
-- generate noise with very precise spectral characteristics.
idctRandAmpPulse
  :: PrimMonad m
  => Mutable.MVector (PrimState m) Sample
  -> TimeWindow Moment -> FDSignal -> TFRandT m ()
idctRandAmpPulse mvec win = mapM_ (randAmpPulse mvec win) . listFDElems

-- | Renders a 'FDSignal' using 'randAmpPulseST', rather than a clean sine wave. This can be used to
-- generate noise with very precise spectral characteristics.
idctRandAmpPulsePure :: Duration -> FDSignal -> IO TDSignal
idctRandAmpPulsePure dur fd = do
  gen <- initTFGen
  let size = durationSampleCount dur
  let win  = TimeWindow { timeStart = 0, timeEnd = dur }
  return TDSignal
    { tdSamples = Unboxed.create $ do
        mvec <- Mutable.new $ size + 1
        flip evalTFRandTWithGen gen $ idctRandAmpPulse mvec win fd
        minMaxVec mvec >>= normalize mvec
        return mvec
    }

-- | Uses the @IO@ monad to generate a random seed, then evaluates the 'randAmpPulseST' function to
-- a pure function value.
randAmpPulsePure :: Frequency -> Duration -> HalfLife -> IO TDSignal
randAmpPulsePure freq dur decay = do
  gen <- initTFGen
  let size = durationSampleCount dur
  let win  = TimeWindow{ timeStart = 0, timeEnd = dur } :: TimeWindow Moment
  return TDSignal
    { tdSamples = Unboxed.create $ do
        mvec <- Mutable.new size
        flip evalTFRandTWithGen gen $ randAmpPulse mvec win FDComponent
          { fdFrequency  = freq
          , fdAmplitude  = 1.0
          , fdPhaseShift = 0.0
          , fdDecayRate  = decay
          , fdNoiseLevel = 0.0
          , fdUndertone  = 0.0
          , fdUnderphase = 0.0
          }
        minMaxVec mvec >>= normalize mvec
        return mvec
    }

-- | Construct a random 'TDSignal' from a random 'FDSignal' constructed around a given base
-- 'ProcGen.Types.Frequency' by the 'randFDSignal' function.
randTDSignalIO :: Duration -> Frequency -> IO TDSignal
randTDSignalIO dt = fmap (pureIDCT dt) . seedIOEvalTFRand . randFDSignal

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


