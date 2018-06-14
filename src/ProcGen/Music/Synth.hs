module ProcGen.Music.Synth
  ( FDSignal, fdSignalVector, emptyFDSignal, nullFDSignal,
    fdSignal, fdSize, fdMinFreq, fdMaxFreq, fdBaseFreq,
    listFDElems, listFDAssocs, lookupFDComponent,
    componentMultipliers, randFDSignal, randFDSignalIO,
    TDSignal, allTDSamples, listTDSamples, tdTimeWindow, tdDuration, idct,
    minMaxTDSignal, randTDSignalIO, writeTDSignalFile, readTDSignalFile,
    FDView(..), fdView, runFDView,
    TDView(..), tdView, runTDView,
    --tdViewInitTime, tdViewAnimate, tdViewFrameCount, tdViewAtTime,
    --resizeTDView, animateTDView, clickMouseTDView, drawTDView,
  ) where

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Music.WaveFile

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Random
import           Control.Monad.ST

import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable
import           Data.Word

import           Linear.V2

import qualified Graphics.Rendering.Cairo    as Cairo

import           Happlets.Lib.Gtk

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
    }
  deriving (Eq, Ord, Show)

emptyFDComponent :: FDComponent
emptyFDComponent = FDComponent
  { fdFrequency  = 0
  , fdAmplitude  = 0
  , fdPhaseShift = 0
  }

-- | Returns 'Prelude.True' if either th frequency or amplitude are zero.
nullFDComponent :: FDComponent -> Bool
nullFDComponent fd = fdFrequency fd == 0 || fdAmplitude fd == 0

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
  deriving (Eq, Show, Read)

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
fdSignal :: ComponentCount -> [FDComponent] -> FDSignal
fdSignal n = filter (not . nullFDComponent) >>> \ case
  []       -> emptyFDSignal
  c0:elems -> runST $ do
    let [grpFreq, grpAmp, grpPhase, grpSize] = [0 .. 3] :: [Int]
    let putVec i c vec = do
          let write j get = Mutable.write vec (i + j) (get c)
          write grpFreq  fdFrequency
          write grpAmp   fdAmplitude
          write grpPhase fdPhaseShift
          return vec
    vec <- Mutable.new (n * 3) >>= putVec 0 c0
    -- Write sampels to array while measuring fdMinFreq, fdMaxFreq, and which frequency has the
    -- biggest amplitude, which will be the 'fdBaseFreq'.
    (ampMax, count, fd) <- foldM
      (\ (ampMax, count, fd) (i, c) ->
         if nullFDComponent c then return (ampMax, count, fd) else do
           putVec i c vec
           count <- pure $! count + 1
           let freq = fdFrequency c
           fd <- pure $ fd
             { fdMinFreq  = min freq $ fdMinFreq fd
             , fdMaxFreq  = max freq $ fdMaxFreq fd
             }
           pure $ if ampMax >= fdAmplitude c then (ampMax, count, fd) else
             (fdAmplitude c, count, fd{ fdBaseFreq = freq })
      )
      (fdAmplitude c0, 1, emptyFDSignal{ fdMinFreq = fdFrequency c0, fdMaxFreq = fdFrequency c0 })
      (zip [0, grpSize .. grpSize * (n - 1)] $ elems ++ repeat emptyFDComponent)
    -- Normalize, such that the base freq has an amplitude of 1 and all other frequencies have an
    -- amplitude proportional to that.
    unless (ampMax == 0) $ forM_
      [grpAmp, grpAmp + grpSize .. grpSize * (n - 1)]
      (\ i -> Mutable.read vec i >>= Mutable.write vec i . (/ ampMax))
    vec <- Unboxed.freeze vec
    return fd{ fdSize = count, fdSignalVector = vec }

-- | Extract a copy of every element triple from the 'FDSignal' as a list.
listFDElems :: FDSignal -> [FDComponent]
listFDElems (FDSignal{fdSignalVector=vec}) = loop $ Unboxed.toList vec where
  loop = \ case
    freq:amp:phase:ax -> FDComponent
      { fdFrequency  = freq
      , fdAmplitude  = amp
      , fdPhaseShift = phase
      } : loop ax
    _ -> []

-- | Similar to 'listFDElems', but includes the integer index associated with each element.
listFDAssocs :: FDSignal -> [(ComponentIndex, FDComponent)]
listFDAssocs = zip [0 ..] . listFDElems

-- | Extract a copy of a single element at a given index.
lookupFDComponent :: FDSignal -> ComponentIndex -> Maybe FDComponent
lookupFDComponent (FDSignal{fdSignalVector=vec}) = (* 3) >>> \ i ->
  FDComponent <$> (vec Unboxed.!? i) <*> (vec Unboxed.!? (i + 1)) <*> (vec Unboxed.!? (i + 2))

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
randFDSignal base = do
  (count, components) <- fmap (first sum . unzip . concat) $ forM compMult $ \ mul -> do
    dice <- getRandom :: TFRand Word8
    if dice > div maxBound 8 + 1 then return [] else do
      amp   <- (* (3/4) ) . (+ (1/3))    <$> getRandom :: TFRand Frequency
      phase <- (* (2*pi)) . subtract 0.5 <$> getRandom :: TFRand PhaseShift
      return $ do
        let freq = base * mul
        guard $ freq < nyquist
        return $ (,) 1 $ FDComponent
          { fdFrequency  = base * mul
          , fdAmplitude  = if mul > 1 then amp / mul else amp * mul
          , fdPhaseShift = phase
          }
  return $! fdSignal (count + 1) $ FDComponent
    { fdFrequency = base
    , fdAmplitude = 1.0
    , fdPhaseShift = 0.0
    } : components

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
  { tdSamples = Unboxed.create $ do
      let n = durationSampleCount dt
      if n <= 0 then Mutable.new 0 else do
        vec <- Mutable.new n
        forM_ [0 .. n-1] $ \ t -> Mutable.write vec t $ sum $ do
          FDComponent{fdFrequency=freq,fdAmplitude=amp,fdPhaseShift=phase} <- listFDElems fd
          guard $ freq <= nyquist
          [amp * sin(phase + 2.0 * pi * freq * indexToTime t)]
        minMaxVec vec >>= normalize vec
        return vec
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

newtype FDView
  = FDView
    { theFDViewSignal :: FDSignal
    }

-- | Another constructor for 'FDView', but has a name consistent with the 'TDView' and 'tdView'
-- constructors.
fdView :: FDSignal -> FDView
fdView = FDView

drawFDView :: FDView -> PixSize -> GtkRedraw ()
drawFDView fdView (V2 w h) = cairoRender $ do
  let fd     = theFDViewSignal fdView
  let lo     = log (fdMinFreq fd)
  let xscale = realToFrac w / (log (fdMaxFreq fd) - lo)
  h <- pure $ realToFrac h
  cairoClearCanvas  1.0  1.0  1.0  0.8
  forM_ (listFDElems fd) $ \ FDComponent{fdFrequency=freq,fdAmplitude=amp} -> do
    let x = realToFrac (round ((log freq - lo) * xscale) :: Int) + 0.5
    let y = realToFrac (1 - amp) * h + 0.5
    Cairo.setSourceRGBA  1.0  0.0  0.0  1.0
    Cairo.moveTo  x  (realToFrac h + 0.5)
    Cairo.lineTo  x  y
    Cairo.stroke
    Cairo.arc     x  y  1.5  0.0  (2.0 * pi)
    Cairo.fill

resizeFDView :: GtkGUI FDView ()
resizeFDView = getModel >>= void . onView . drawFDView

runFDView :: GtkGUI FDView ()
runFDView = do
  resizeEvents $ const resizeFDView
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
    { theTDViewAnimate     :: !Bool
    , theTDViewSignal      :: !TDSignal
      -- ^ The signal to visualize.
    , theTDViewBaseFreq    :: !Frequency
      -- ^ The frequency at which a redraw is triggered.
    , theTDViewSampleCount :: !SampleCount
      -- ^ A signal sampled at 44100 Hz animated at 60 FPS means each animation frame can depict
      -- exactly 735 samples in real time. However if you prefer to have a 1-sample = 1-pixel
      -- visualization of the 'TDSignal' regardless of the actual GUI window size, set this value to
      -- the width of the GUI window whenever it is resized.
    , theTDViewInitTime    :: !Moment
      -- ^ The time within the 'TDSignal' at which we begin drawing the graphic at the left-most
      -- edge of the screen.
    , theTDViewFrameCount  :: !Int
      -- ^ This value does nothing for the visualization, it is updated by the animation event
      -- handler, and simply counts how many animation frames have elapsed.
    }

-- | Constructs a new controller containing a 'TDView' from a 'TDSignal' and it's base
-- 'ProcGen.Types.Frequency'.
tdView :: TDSignal -> Frequency -> TDView
tdView td f = TDView
  { theTDViewAnimate     = False
  , theTDViewSignal      = td
  , theTDViewBaseFreq    = f
  , theTDViewSampleCount = round $ sampleRate / animationRate
  , theTDViewInitTime    = 0
  , theTDViewFrameCount  = 0
  }

tdViewInitTime :: Lens' TDView Moment
tdViewInitTime = lens theTDViewInitTime $ \ a b -> a{ theTDViewInitTime = b }

tdViewAnimate :: Lens' TDView Bool
tdViewAnimate = lens theTDViewAnimate $ \ a b -> a{ theTDViewAnimate = b }

tdViewFrameCount :: Lens' TDView Int
tdViewFrameCount = lens theTDViewFrameCount $ \ a b -> a{ theTDViewFrameCount = b }

-- | Shift the 'tdViewInitTime' so that the time is at least at the given 'ProcGen.Types.Moment',
-- and also shift forward a bit to make sure the 'tdViewInitTime' is on an integer number multiple
-- of the 'tdViewBaseFreq'. Returns whether the 'ProcGen.Types.Moment' given is beyond the end of
-- the 'TDSignal'.
tdViewAtTime :: Moment -> GtkGUI TDView ()
tdViewAtTime t = do
  v <- getModel
  if t >= tdDuration (theTDViewSignal v)
   then (tdViewAnimate .= False) >> disable
   else do
     tdViewFrameCount %= (+ 1)
     tdViewInitTime .=
       (realToFrac (ceiling (t * theTDViewBaseFreq v) :: Int) / theTDViewBaseFreq v)

drawTDView :: TDView -> PixSize -> GtkRedraw ()
drawTDView v (V2 (SampCoord w) (SampCoord h)) = cairoRender $ do
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
  Cairo.moveTo  0.0 $ f $ theTDViewInitTime v
  forM_ (realToFrac <$> [1 .. count - 1]) $ \ pix -> do
    let x = pixTime * pix
    let t = realToFrac x / sampleRate + theTDViewInitTime v
    let y = f t
    Cairo.lineTo x y
  Cairo.stroke

resizeTDView :: GtkGUI TDView ()
resizeTDView = getModel >>= void . onView . drawTDView

animateTDView :: AnimationMoment -> GtkGUI TDView ()
animateTDView = realToFrac >>> \ dt -> do
  beyond <- gets $ (>=) dt . tdDuration . theTDViewSignal
  if beyond then stepFrameEvents $ const disable else do
    tdViewAtTime dt
    getModel >>= void . onView . drawTDView

clickMouseTDView :: Mouse -> GtkGUI TDView ()
clickMouseTDView (Mouse _ pressed _mod button _loc) = when pressed $ case button of
  RightClick -> do
    modifyModel $ (tdViewInitTime .~ 0) . (tdViewAnimate .~ False)
    getModel >>= void . onView . drawTDView
  _          -> do
    tdViewAnimate %= not -- This couldn't possibly toggle the animate bit.... NOT!!!
    isNowAnimated <- use tdViewAnimate
    stepFrameEvents $ if isNowAnimated then animateTDView else const disable

-- | Use this to attach a @('Happlets.GUI.Happlet' 'TDView')@ to a window.
runTDView :: GtkGUI TDView ()
runTDView = do
  mouseEvents MouseButton clickMouseTDView
  resizeEvents $ const resizeTDView
  resizeTDView
