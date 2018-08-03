module ProcGen.Music.Synth
  ( FDComponent(..), emptyFDComponent,
    FDComponentList, randFDComponents, fdComponentInsert,
    FDSignalDefinition(..),
    FDSigDefinitionElem(..),
    FDSigShapeFreqSel(..),
    FDSigShape(..),
    FDSigShapeElem(..),
    FDSignal, fdSignalVector, emptyFDSignal, nullFDSignal,
    fdSignal, fdSize, fdMinFreq, fdMaxFreq, fdBaseFreq,
    listFDElems, listFDAssocs, lookupFDComponent,
    componentMultipliers, randFDSignal, randFDSignalIO,
    randAmpPulseST, randAmpPulse,
    TDSignal, allTDSamples, listTDSamples, tdTimeWindow, tdDuration,
    idct, idctST, idctRandAmpPulse, idctRandAmpPulseST,
    minMaxTDSignal, randTDSignalIO, writeTDSignalFile, readTDSignalFile,
    FDView(..), fdView, runFDView,
    TDView(..), tdView, runTDView,
    --tdViewInitTime, tdViewAnimate, tdViewFrameCount, tdViewAtTime,
    --resizeTDView, animateTDView, clickMouseTDView, drawTDView,
    CreateRandomFD(..), DrawingMode(..),
  ) where

import           Happlets.Lib.Gtk

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Collapsible
import           ProcGen.Music.WaveFile
import           ProcGen.Properties
import           ProcGen.VectorBuilder

import           Control.Arrow
import           Control.Monad.ST

import           Data.Semigroup
import qualified Data.Vector                 as Boxed
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable
import           Data.Word

import           Linear.V2

import qualified Graphics.Rendering.Cairo    as Cairo

import           Text.Printf

----------------------------------------------------------------------------------------------------

-- | Used to count the number of component frequencies in an 'FDSignal'
type ComponentCount = Int

-- | Used to select a single component frequency from an 'FDSignal'
type ComponentIndex = Int

data FDComponent
  = FDComponent
    { fdFrequency  :: !Frequency
    , fdAmplitude  :: !Amplitude
    , fdPhaseShift :: !PhaseShift
    , fdDecayRate  :: !HalfLife
    }
  deriving (Eq, Ord)

instance Show FDComponent where
  show fd = printf "(FD freq=%+.4f amp=%+.4f phase=%+.4f decay=%+.4f)"
    (fdFrequency fd) (fdAmplitude fd) (fdPhaseShift fd) (fdDecayRate fd)

instance Collapsible Float FDComponent where
  collapse =
    buildRecord fdFrequency <>
    buildRecord fdAmplitude <>
    buildRecord fdPhaseShift <>
    buildRecord fdDecayRate
  uncollapse = error "TODO: (uncollapse :: UVec.Vector -> FDComponent)"

emptyFDComponent :: FDComponent
emptyFDComponent = FDComponent
  { fdFrequency  = 0
  , fdAmplitude  = 0
  , fdPhaseShift = 0
  , fdDecayRate  = 0
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
fdComponentAmplitudeAt (FDComponent{fdDecayRate=hl,fdAmplitude=amp}) t = amp *
  let thl = if hl <= 0.0 then 1.0 else t / hl + 1.0 in 1.0 / thl

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
      phase <- onRandFloat $ (* (2*pi)) . subtract 0.5
      decay <- onRandFloat (* 2)
      return $ do
        let freq = base * mul
        guard $ freq < nyquist
        guard $ amp  > 0.1
        return $ (,) 1 $ FDComponent
          { fdFrequency  = base * mul
          , fdAmplitude  = if mul > 1 then amp / mul else amp * mul
          , fdPhaseShift = phase
          , fdDecayRate  = decay
          }
  return FDComponentList
    { theFDCompListLength = count + 1
    , theFDCompListElems  = FDComponent
        { fdFrequency  = base
        , fdAmplitude  = 1.0
        , fdPhaseShift = 0.0
        , fdDecayRate  = 0.0
        } : components
    }

fdComponentInsert :: FDComponent -> FDComponentList -> FDComponentList
fdComponentInsert c list = FDComponentList
  { theFDCompListElems  = c : theFDCompListElems  list
  , theFDCompListLength = 1 + theFDCompListLength list
  }

----------------------------------------------------------------------------------------------------

-- | This is a randomizable data structure that can define various kinds of spectral "shapes" of a
-- signal. These shapes can then be rendered to a 'FDSignal' using 'renderFDShape'.
--
-- When rendering a shape, there is always a reference frequency which always begins at t0 at 100%
-- amplitude. All other components have random amplitudes, but the randomization is "shaped" by this
-- data type.
--
-- The 'FDSigDefinition' is composed of a few fundamental elements that can be superimposed to
-- construct the final 'FDSignal' rendering.
--
-- The 'FDSigDefinition' data itself does not contain a reference frequency, rather when rendering
-- to a 'FDSignal' a reference frequency is specified and is applied to every rendering function
-- that generates the 'FDComponent's.
newtype FDSignalDefinition
  = FDSignalDefinition{ unwrapFDSignalDefinition :: Boxed.Vector FDSigDefinitionElem }
  deriving (Eq)

-- | A 'FDSigShape' is constructed from one or more elements, where each element is a fundamental
-- probability distribution (uniform, normal, quadratic) with it's own frequency and amplitude
-- distribution characteristics.
data FDSigDefinitionElem
  = FDSigDefinitionElem
    { sigShapeFreqSel :: !FDSigShapeFreqSel
      -- ^ Select the frequencies for this element.
    , sigShapeLevels  :: !FDSigShape
      -- ^ Shape the amplitude of the selected frequencies by semi-randomly selecting amplitude
      -- levels for each frequency.
    }
  deriving (Eq)

-- | A frequency selector.
data FDSigShapeFreqSel
  = FDSigFreqSelArbitrary !Int !Int
    -- ^ select more than or equal to @(lo :: Int)@ and less than or equal to @(hi :: Int)@
    -- frequencies completely randomly.
  | FDSigFreqSelRationals
    { sigFreqSelNumerators   :: !(Unboxed.Vector Word8)
    , sigFreqSelDenominators :: !(Unboxed.Vector Word8)
    , sigFreqSelRandomizer   :: !FDSigFreqSelRandomizer -- ^ Maybe randomly limit the selection.
    } -- ^ compose a list of rationals consisting of all given numerators divided by all given
      -- denominators, these rationals will be multiplied by the reference frequency to generate the
      -- frequencies for this shape element.
  deriving (Eq)

-- | This data type is used by the 'FDSigFreqSelRationals' constructor of the 'FDSigShapeFreqSel'
-- data type. The 'FDSigFreqSelRationals' selector selects frequencies from a list of rational
-- scalar multiples of the base frequency. 
data FDSigFreqSelRandomizer
  = FDSigFreqSelAll -- ^ Select all frequencies, do not randomly choose a subset of them.
  | FDSigFreqSelRandomizer !Word8 !Word8
    -- ^ Randomly choose a subset of frequencies containing a some number between
    -- @(lo :: 'Data.Word.Word8')@ to @(hi :: 'Data.Word.Word8')@ elements.
  deriving (Eq)

-- | This data type defines how to randomized the "shape" of the amplitude levels of the various
-- frequencies distributed across the audible spectrum.
data FDSigShape
  = FDSigShape
  { sigShapeOvertones  :: !(Boxed.Vector FDSigShapeElem)
    -- ^ Describes the frequencies higher the reference frequency.
  , sigShapeUndertones :: !(Boxed.Vector FDSigShapeElem)
    -- ^ Describes the freqeuncies lower than the reference frequency.
  }
  deriving (Eq)

data FDSigShapeElem
  = FDSigShapeElem
    { sigShapeBandwidth :: !Probability
      -- ^ What percentage of bandwidth along the audible frequency band does this particular
      -- primitive apply it's shape.
    , sigShapePrimitive :: !FDSigShapePrimitive
      -- ^ The primitive function that generates the shape.
    }
  deriving (Eq)

-- | This data type contains a few functions for shaping the amplitude levels of a set of
-- frequencies. One or more of these defines the shape of all amplitudes along the frequency
-- distribution.
data FDSigShapePrimitive
  = FDSigShapeLiner
    -- ^ All frequencies draw a random amplitude between 0.0 and the reciporical of the reference
    -- frequency.
  | FSSigShapeUniformDist !Amplitude !Amplitude
    -- ^ All frequencies draw a random amplitude between @(aMin :: 'Amplitude')@ and
    -- @(aMax :: 'Amplitude').
  | FDSigShapeNormal    !Bandwidth
    -- ^ Bell curve shape with a given bandwidth variance around the reference frequency.
  | FDSigShapeTrinomial !ProcGenFloat !ProcGenFloat !ProcGenFloat !ProcGenFloat
    -- ^ Quadratic curve shape with four parameters @(a :: 'ProcGenFloat')@,
    -- @(b :: 'ProcGenFloat')@, @(c :: 'ProcGenFloat')@, @(d :: 'ProcGenFloat'), and the shape of
    -- this curve is given by the equation: @(\ t -> a*t^3 + b*t^2 + c*t + d)@, where the amplitude
    -- of a frequency value @f@ and the reference frequency @r@, the value of @t@ passed to this
    -- function is @t = r - f@, meaning the value t0 applied to this trinomial function describes
    -- the amplitude of the reference frequency.
  deriving (Eq)

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
    freq:amp:phase:decay:ax -> FDComponent
      { fdFrequency  = freq
      , fdAmplitude  = amp
      , fdPhaseShift = phase
      , fdDecayRate  = decay
      } : loop ax
    _ -> []

-- | Similar to 'listFDElems', but includes the integer index associated with each element.
listFDAssocs :: FDSignal -> [(ComponentIndex, FDComponent)]
listFDAssocs = zip [0 ..] . listFDElems

-- | Extract a copy of a single element at a given index.
lookupFDComponent :: FDSignal -> ComponentIndex -> Maybe FDComponent
lookupFDComponent (FDSignal{fdSignalVector=vec}) = (* 4) >>> \ i -> FDComponent
  <$> (vec Unboxed.!? (i + 0))
  <*> (vec Unboxed.!? (i + 1))
  <*> (vec Unboxed.!? (i + 2))
  <*> (vec Unboxed.!? (i + 3))

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
randFDSignalIO = evalTFRandIO . randFDSignal

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
idct :: Duration -> FDSignal -> TDSignal
idct dt fd = TDSignal
  { tdSamples = let n = durationSampleCount dt in Unboxed.create $
      if n <= 0 then Mutable.new 0 else do
        mvec <- Mutable.new $ n + 1
        idctST mvec (TimeWindow{ timeStart = 0, timeEnd = dt }) fd
        minMaxVec mvec >>= normalize mvec
        return mvec
  }

-- | Perform the 'idct' function as a type of @'Control.Monad.ST.ST'@ function so that it can be use
-- cumulatively on 'Mutable.STVector's. WARNING: the results are not normalized, so you will almost
-- certainly end up with elements in the vector that are well above 1 or well below -1. Evaluate
-- @\ mvec -> 'minMaxVec' mvec >>= normalize 'mvec'@ before freezing the 'Mutable.STVector'.
idctST :: Mutable.STVector s Sample -> TimeWindow Moment -> FDSignal -> ST s ()
idctST mvec win fd =
  forM_ (twIndicies (Mutable.length mvec) win) $ \ i -> Mutable.write mvec i $! sum $ do
    fd <- listFDElems fd
    guard $ fdFrequency fd <= nyquist
    [fdComponentSampleAt fd $ indexToTime i]

-- | Accumulates a signal into an 'Mutable.STVector' that consists of a single sine wave, but the
-- amplitude of the wave is randomly modified for every cycle. This creates noise with a very narrow
-- frequency spectrum.
randAmpPulseST
  :: Mutable.STVector s Sample
  -> TimeWindow Moment -> FDComponent -> TFRandT (ST s) ()
randAmpPulseST mvec win
  (FDComponent{fdFrequency=freq,fdAmplitude=amp0,fdPhaseShift=phase,fdDecayRate=decay}) = do
    let ti = fst . timeIndex
    let size = ti (3 / freq) + 1
    -- A table of values is used here because we are not simply summing sine waves, we are creating a
    -- sine wave with a sigmoidal fade-in and fade-out, which requires two Float32 multiplications,
    -- two calls to 'exp', two calls to 'recip', and one calls to 'sin' for each index. It is probably
    -- faster on most computer hardware to store these values to a table rather than compute them on
    -- the fly.
    table <- lift $ Mutable.new size
    lift $ forM_ [0 .. size - 1] $ \ i ->
      Mutable.write table i $ sinePulse3 freq (0.0) $ indexToTime i
    let t0 = ((\ p -> if p < 0 then p + freq else p) $ contMod phase freq) + timeStart win
    let loop amp i j =
          if i >= Mutable.length mvec || j >= Mutable.length table then return () else do
            let decamp = amp0 * amp * decay ** (indexToTime i - t0)
            lift $ Mutable.write mvec i =<<
              liftM2 (+) (Mutable.read mvec i) ((* decamp) <$> Mutable.read table j)
            loop amp (i + 1) (j + 1)
    let pulses t = if t >= timeEnd win then return () else do
          amp <- getRandom :: TFRandT (ST s) Float
          let i = fst $ timeIndex t
          loop amp i 0
          pulses $! t + 2 / freq
    pulses t0

-- | Renders a 'FDSignal' using 'randAmpPulseST', rather than a clean sine wave. This can be used to
-- generate noise with very precise spectral characteristics.
idctRandAmpPulseST
  :: Mutable.STVector s Sample
  -> TimeWindow Moment -> FDSignal -> TFRandT (ST s) ()
idctRandAmpPulseST mvec win = mapM_ (randAmpPulseST mvec win) . listFDElems

-- | Renders a 'FDSignal' using 'randAmpPulseST', rather than a clean sine wave. This can be used to
-- generate noise with very precise spectral characteristics.
idctRandAmpPulse :: Duration -> FDSignal -> IO TDSignal
idctRandAmpPulse dur fd = do
  gen <- initTFGen
  let size = durationSampleCount dur
  let win  = TimeWindow { timeStart = 0, timeEnd = dur }
  return TDSignal
    { tdSamples = Unboxed.create $ do
        mvec <- Mutable.new $ size + 1
        flip evalTFRandT gen $ idctRandAmpPulseST mvec win fd
        minMaxVec mvec >>= normalize mvec
        return mvec
    }

-- | Uses the @IO@ monad to generate a random seed, then evaluates the 'randAmpPulseST' function to
-- a pure function value.
randAmpPulse :: Frequency -> Duration -> HalfLife -> IO TDSignal
randAmpPulse freq dur decay = do
  gen <- initTFGen
  let size = durationSampleCount dur
  let win  = TimeWindow{ timeStart = 0, timeEnd = dur } :: TimeWindow Moment
  return TDSignal
    { tdSamples = Unboxed.create $ do
        mvec <- Mutable.new size
        flip evalTFRandT gen $ randAmpPulseST mvec win FDComponent
          { fdFrequency = freq, fdAmplitude = 1.0, fdPhaseShift = 0.0, fdDecayRate = decay }
        minMaxVec mvec >>= normalize mvec
        return mvec
    }

-- | Construct a random 'TDSignal' from a random 'FDSignal' constructed around a given base
-- 'ProcGen.Types.Frequency' by the 'randFDSignal' function.
randTDSignalIO :: Duration -> Frequency -> IO TDSignal
randTDSignalIO dt = fmap (idct dt) . evalTFRandIO . randFDSignal

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


