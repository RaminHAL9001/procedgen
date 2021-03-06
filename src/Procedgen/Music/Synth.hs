-- | This module defines the data types and functions for creating sound effects that can be used as
-- musical instruments.
module Procedgen.Music.Synth
  ( -- * Signal Generating Functions
    SynthSigComponent, SynthSignal(..), SynthNoise(..), synthFDComponent,
    -- * A language for defining musical sounds
    Synth(..), SynthState(..), initSynthIO, runSynth,
    resizeSynthBuffer, resetSynthBuffer,
    synthFDSignal, synthFDNoise,
    synthAttack, synthFrequency, synthBuffer,
    -- * Constructing Frequency Domain Components
    smallFractions, bigFractions, allFractions, fractions,
    -- * Shaping Frequency Component Levels
    synthRandomizeLevels,
  ) where

import           Happlets.Lib.Gtk

import           Procedgen.Types
import           Procedgen.Arbitrary
import           Procedgen.Music.AudioSignal
import qualified Procedgen.Music.TDBezier      as TDBezier

import           Data.List         (nub)
import           Data.Semigroup
--import qualified Data.Text                   as Strict
import qualified Data.Vector                 as Boxed
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable

----------------------------------------------------------------------------------------------------

newtype Synth a = Synth { unwrapSynth :: StateT SynthState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState SynthState Synth where { state = Synth . state; }
instance Semigroup a => Semigroup (Synth a) where { a <> b = (<>) <$> a <*> b; }
instance Monoid a => Monoid (Synth a) where
  mappend a b = mappend <$> a <*> b
  mempty = pure mempty

instance MonadRandom Synth where
  getRandomR  = synthLiftTFRandT  . getRandomR
  getRandom   = synthLiftTFRandT getRandom
  getRandomRs = synthLiftTFRandT . getRandomRs
  getRandoms  = synthLiftTFRandT getRandoms

synthLiftTFRandT :: TFRandT IO a -> Synth a
synthLiftTFRandT f = use synthTFGen >>=
  liftIO . runTFRandT f >>= \ (a, gen) -> synthTFGen .= gen >> return a

----------------------------------------------------------------------------------------------------

data SynthState
  = SynthState
    { theSynthSustain     :: !(Boxed.Vector SynthSignal)
      -- ^ Elements to be used to render the sustain of an 'FDSignal'.
    , theSynthAttack      :: !(Boxed.Vector SynthNoise)
    , theSynthFrequency   :: !Frequency
      -- ^ The frequency around which procedurally generated frequency domain components are
      -- centered.
--    , theSynthFDShape     :: !FDSignalShape
--      -- ^ a function used to enforce a shape on procedurally generated frequency domain components.
    , theSynthBuffer      :: !(Mutable.IOVector Sample)
      -- ^ a buffer used to render the frequency domain signal to a time domain signal. This buffer
      -- begins with enough memory to store 4.0 seconds of sound, but can be changed with
      -- 'resetSynthBuffer' and 'resizeSynthBuffer'.
    , theSynthTFGen       :: !TFGen
      -- ^ A random number generator state for the /"Two-Fish"/ PRNG algorithm.
    }

---- | An element is a frozen vector of 'FDComponents' with a name and a few other properties that can
---- be used to display it in a GUI.
--data SynthElement
--  = SynthElement
--    { theSynthElemLabel    :: !Strict.Text
--      -- ^ A textual label that can be used to manipulate this element when it is in a list.
--    , theSynthElemColor    :: !Color
--      -- ^ When rendering this element, what color should be used to draw it?
--    , theSynthElemIsStrike :: !Bool
--      -- ^ Strike elements are always rendered first and are always rendered with a brief
--      -- envelope. A strike is the sound made when an instrument is struck, either with an implement
--      -- like a finger, a stick, a pick, or a bow, or with air in the case of woodwind and brass
--      -- instruments.
--    , theSynthElemSignal   :: !FDSignal
--      -- ^ The 'FDSignal' content of this element.
--    }

--instance BufferIDCT SynthElement where
--  bufferIDCT mvec win = bufferIDCT mvec win . theSynthElemSignal

--instance BufferIDCT SynthState where
--  bufferIDCT mvec win = mapM_ (bufferIDCT mvec win) . theSynthElemSignal

--  synthElemLabel, synthElemColor, synthElemIsStrike, synthElemSignal,
--  synthElements, synthTFGen, synthFDShape, synthBuffer, synthFrequency,

---- | The elements we are working with.
--synthSustain :: Lens' SynthState (SynthSignalGroup SynthSignal)
--synthSustain = lens theSynthSustain $ \ a b -> a{ theSynthSustain = b }

synthAttack :: Lens' SynthState (Boxed.Vector SynthNoise)
synthAttack = lens theSynthAttack $ \ a b -> a{ theSynthAttack = b }

synthTFGen :: Lens' SynthState TFGen
synthTFGen = lens theSynthTFGen $ \ a b -> a{ theSynthTFGen = b }
 
--synthFDShape :: Lens' SynthState FDSignalShape
--synthFDShape = lens theSynthFDShape $ \ a b -> a{ theSynthFDShape = b }

synthFrequency :: Lens' SynthState Frequency
synthFrequency = lens theSynthFrequency $ \ a b -> a{ theSynthFrequency = b }

synthBuffer :: Lens' SynthState (Mutable.IOVector Sample)
synthBuffer = lens theSynthBuffer $ \ a b -> a{ theSynthBuffer = b }

--synthElemLabel    :: Lens' SynthElement Strict.Text
--synthElemLabel = lens theSynthElemLabel $ \ a b -> a{ theSynthElemLabel = b }
--
--synthElemColor    :: Lens' SynthElement Color
--synthElemColor = lens theSynthElemColor $ \ a b -> a{ theSynthElemColor = b }
--
--synthElemIsStrike :: Lens' SynthElement Bool
--synthElemIsStrike = lens theSynthElemIsStrike $ \ a b -> a{ theSynthElemIsStrike = b }
--
--synthElemSignal   :: Lens' SynthElement FDSignal
--synthElemSignal = lens theSynthElemSignal $ \ a b -> a{ theSynthElemSignal = b }

----------------------------------------------------------------------------------------------------

-- | A 'FDComponent' generating functions, where each parameter of an 'FDComponent' is a function
-- from a 'Frequency' to some 'ProcGenFloat' value.
type SynthSigComponent = TDBezier.Ord3Spline

-- | Use the 'synthFDComponent' with the parameters set in this data structure to generate
-- generating a single 'FDComponent'. Each component is a bezier 'TDBezier.Ord3Spline' (spline of
-- order-3, better known as a Cubic Bezier Spline) which acts as a function from 'Frequency' to some
-- 'ProcGenFloat' value. This allows the components to vary predictably around a given input
-- frequency.
data SynthSignal
  = SynthSignal
    { synthSigFreqCoef   :: !FreqCoeficient
    , synthSigFrequency  :: !SynthSigComponent
    , synthSigAmplitude  :: !SynthSigComponent
    , synthSigPhaseShift :: !SynthSigComponent
    , synthSigDecayRate  :: !SynthSigComponent
    , synthSigNoiseLevel :: !SynthSigComponent
    , synthSigUndertone  :: !SynthSigComponent
    , synthSigUnderphase :: !SynthSigComponent
    , synthSigUnderamp   :: !SynthSigComponent
    }
  deriving Eq

-- | This data type is used to define the attack of an instrumental sound effect. It is essentially
-- a single normal distribution with a given amplitude, variance, and median value somehwere along
-- the frequency spectrum where random components are most likely to appear. A group of these in a
-- 'SynthSignalGroup' allows you to define approximate most arbitrary noise envelopes.
data SynthNoise
  = SynthNoise
    { synthNoiseFrequency     :: !SynthSigComponent
      -- ^ This function produces a multiplier at a given base frequency which is then multiplied by
      -- that base frequency to produce the actual frequency around which the noise components are
      -- generated.
    , synthNoiseFrequencyCoef :: ProcGenFloat
      -- ^ The 'synthNoiseFrequency' function is normalized to produce a value between 0 and 1. To
      -- get an actual frequency multiplier value, multiply the result of 'synthNoiseFrequency' by
      -- this coeficient value.
    , synthNoiseAmplitude     :: !SynthSigComponent
    , synthNoiseVariance      :: !SynthSigComponent
    , synthNoiseDensity       :: !SynthSigComponent
      -- ^ This is a function to compute the number of random noise components to be generated at
      -- any given frequency.
    , synthNoiseDensityCoef   :: !ProcGenFloat
      -- ^ Multiplies the 'synthNoiseDensity' value times this value and rounds to produce a number
      -- of components. This number of random components will be generated at the given 'Frequency'.
      -- This value allows the 'synthNoiseDensity' to be normalized to a value between 0 and 1 and
      -- then scaled up.
    }

-- | Construct a new 'FDSignal' from a given 'SynthSignalGroup' of 'SynthNoise' values around a
-- given base 'Frequency'.
synthFDNoise :: Frequency -> Boxed.Vector SynthNoise -> Synth FDSignal
synthFDNoise base grp = do
  fmap (fdSignal base . mconcat) $ forM (Boxed.toList grp) $ \ noise -> do
    let fcoef  = sample (synthNoiseFrequency noise) base * synthNoiseFrequencyCoef noise
    let amp    = sample (synthNoiseAmplitude noise) base
    --let var    = sample (synthNoiseVariance  noise) base
    let nelems = round $ sample (synthNoiseDensity noise) base * synthNoiseDensityCoef noise
    fmap (fdComponentList nelems) $ replicateM nelems $ do
      freq  <- onRandFloat $ FreqCoeficient . (* (2*fcoef)) . inverseNormal
      amp   <- onRandFloat (\ a -> (1+a/2)*amp)
      phase <- onRandFloat id
      return $ emptyFDComponent &~ do
        fdFreqCoef   .= freq
        fdAmplitude  .= amp
        fdPhaseShift .= phase

-- | Construct a new 'FDSignal' from a given 'SynthSignalGroup' of 'SynthSignal' values around a
-- given base 'Frequency'.
synthFDSignal :: Frequency -> Boxed.Vector SynthSignal -> FDSignal
synthFDSignal base grp = fdSignal base $ fdComponentList (Boxed.length grp) $
  synthFDComponent base <$> Boxed.toList grp

-- | Construct a new 'FDComponent' from a given 'SynthSignal' around a given 'Frequency'. This
-- function is usually not as useful as the 'synthFDSignal' function which converts a group of
-- 'SynthSignals' in a 'SynthSignalGroup' into a 'FDSignal'.
synthFDComponent :: Frequency -> SynthSignal -> FDComponent
synthFDComponent base signal = emptyFDComponent &~ do
  let comp lens get = lens .= sample (get signal) base
  fdFreqCoef .= synthSigFreqCoef signal
  comp fdAmplitude  synthSigAmplitude
  comp fdPhaseShift synthSigPhaseShift
  comp fdDecayRate  synthSigDecayRate
  comp fdNoiseLevel synthSigNoiseLevel
  comp fdUndertone  synthSigUnderamp
  comp fdUnderphase synthSigUnderphase
  comp fdUnderamp   synthSigUnderamp  

----------------------------------------------------------------------------------------------------

-- | Evaluate 'SynthState' in the IO monad, constructing the pseudo-random number generator from the
-- operating system's initializer.
initSynthIO :: IO SynthState
initSynthIO =
  SynthState mempty mempty 256.0 <$> newSampleBuffer 4.0 <*> initTFGen

-- | Evaluate a 'Synth' function.
runSynth :: Synth a -> SynthState -> IO (a, SynthState)
runSynth (Synth f) = runStateT f

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
    LT -> assign synthBuffer =<<
      liftIO (Mutable.clone $ Mutable.slice (timeStart iw) (timeEnd iw) oldbuf)

----------------------------------------------------------------------------------------------------

-- | Select the Nth prime number from the 'Procedgen.PrimeNumbers.all16BitPrimes' table.
primeFunc :: Int -> ProcGenFloat
primeFunc = realToFrac . (all16BitPrimes Unboxed.!)

-- | This function generates a list of prime fractions, where the numerator and denominator are both
-- prime numbers, from a selection of primes passed as parameters to this function. To select a
-- prime number, simply pass an 'Prelude.Int' and it will be selected from a table of primes, where
-- the integer @0@ selects the first prime number @2@, the integer @1@ selects the next prime number
-- 3 and so on.
--
-- Pass two selections of primes as a list of 'Prelude.Int's, one for the numerators of the
-- generated fractions, and one for the denominators. As the first parameter to this function, pass
-- a filter function of type @('Prelude.Int' -> 'Prelude.Int' -> 'Prelude.Bool') to filter out
-- numerator/denominator pairs to limit the selection of primes. Passing @('Prelude.==')@ as the
-- filter will result in an empty list, since fractions where the numerator and denominator are
-- equivalent produce a fraction with a value of 1.0, and values of 1.0 are filtered out on the
-- final pass.
fractions :: (Int -> Int -> Bool) -> [Int] -> [Int] -> [ProcGenFloat]
fractions filt nums denoms =
  [ primeFunc num / primeFunc denom
  | denom <- nub denoms, num <- nub nums, num /= denom, filt num denom
  ]

-- | Generate all possible pairs of integers and evaluate them as fractions. Fractions equal to 1.0
-- are not returned. Only return fractions less than 1.0 and greater than 0.0.
smallFractions :: [Int] -> [Int] -> [ProcGenFloat]
smallFractions = fractions (<)

-- | Generate all possible pairs of integers and evaluate them as fractions. Fractions equal to 1.0
-- are not returned. Only return fractions greater than 1.0.
bigFractions :: [Int] -> [Int] -> [ProcGenFloat]
bigFractions = fractions (>)

-- | Generate all possible pairs of integers and evaluate them as fractions. Fractions equal to 1.0
-- are not returned. Only return fractions less than 1.0 and greater than 0.0
allFractions :: [Int] -> [Int] -> [ProcGenFloat]
allFractions = fractions (const $ const True)

---- | Generate a list of 'FDComponents' with fractional multiples of the current base frequency
---- 'synthFrequency', and shaped to the amplitude of the current 'synthFDShape'. Pass the fractions
---- generated by 'allFractions', 'bigFractions', or 'smallFractions'.
--synthMultBaseFreq :: [ProcGenFloat] -> Synth FDComponentList
--synthMultBaseFreq fracs = do
--  shape <- use synthFDShape
--  base  <- use synthFrequency
--  liftM fdComponentList $ mapM (\ comp -> (\ phase -> comp & fdPhaseShift .~ phase) <$> onRandFloat id)
--    [ emptyFDComponent &~ do
--        fdFrequency .= freq
--        fdAmplitude .= applyFDSignalShape shape freq
--    | frac <- fracs, let freq = frac * base
--    ]

----------------------------------------------------------------------------------------------------

-- | Multiply each 'FDComponent' in the given list by a random number between two given 'Amplitude'
-- values.
synthRandomizeLevels :: Amplitude -> Amplitude -> FDComponentList -> Synth FDComponentList
synthRandomizeLevels a b comps = do
  let (lo, hi) = (min 1.0 $ min a b, max 0.0 $ max a b)
  let scale x = if a == b then return (x * a) else onRandFloat (\ c -> (c * (hi - lo) + lo) * x)
  forEachFDComponent comps $ \ comp ->
    scale (comp ^. fdAmplitude) >>= \ amp -> pure (comp & fdAmplitude .~ amp)

---- | A data type specifically used by the 'synthApplyShape' function.
--data ApplyShapeTo
--  = ToAmplitude
--  | ToDecayRate
--  | ToNoiseLevel
--  | ToUndertone
--  | ToUnderamp
--  deriving (Eq, Ord, Show, Read, Enum)

---- | This function maps over each 'FDComponent' and applies the 'fdFrequency' of each component to a
---- 'FDSignalShape' function to produce a new 'Procedgen.Types.ProcGenFloat' value, then applies some
---- noise, then writes or updates this new value into the field of 'FDComponent' according to which
---- 'ApplyToShape' is given.
--synthApplyShape
--  :: ApplyShapeTo        -- ^ which field of the 'FDComponent' to modify
--  -> Synth FDSignalShape -- ^ the shape to use, usually @('Control.Lens.use' 'synthFDShape')@
--  -> NoiseLevel          -- ^ how much noise to apply to the shape, pass 0.0 for a perfect shape.
--  -> (ProcGenFloat -> ProcGenFloat -> ProcGenFloat)
--     -- ^ a function to apply the new value to the old value, usually pass @('Prelude.*')@ or
--     -- 'Prelude.const'. The new value is passed as the first (left-hand) parameter.
--  -> FDComponentList     -- ^ the list of elements to which changes should be applied.
--  -> Synth FDComponentList
--synthApplyShape applyTo getShape noiseLevel cross comps = do
--  shape <- getShape
--  noiseLevel <- pure $ max 0.0 $ min 1.0 $ abs noiseLevel
--  let getNoise = if noiseLevel == 0 then return 1.0 else onRandFloat $ (1.0 -) . (noiseLevel *)
--  forEachFDComponent comps $ \ comp -> do
--    let field = case applyTo of
--          ToAmplitude  -> fdAmplitude
--          ToDecayRate  -> fdDecayRate
--          ToNoiseLevel -> fdNoiseLevel
--          ToUnderamp   -> fdUnderamp
--          ToUndertone  -> fdUndertone
--    noise <- getNoise
--    pure $ comp & field %~ min 1.0 . max 0.0 . abs .
--      cross (noise * applyFDSignalShape shape (comp ^. fdFrequency))

---- | Take a cluster of 'FDComponent's and form a 'FDSignal', then push this 'FDSignal' as a
---- 'SynthElement' on the stack of elements.
--synthPushNewElem
--  :: Strict.Text -- ^ a label by which you can refer to this component later.
--  -> Color       -- ^ the color to use when drawing an image of this element in the GUI.
--  -> Bool        -- ^ whether this element is a strike element.
--  -> SynthSignal
--  -> Synth ()
--synthPushNewElem label color strike comp = synthElements %= (:) SynthElement
--  { theSynthElemLabel    = label
--  , theSynthElemColor    = color
--  , theSynthElemIsStrike = strike
--  , theSynthElemSignal   = comp
--  }

---- | Evaluate a mapping function on the top-most 'SynthElement in the stack. This function does
---- nothing if the 'SynthElement' stack is empty.
--synthOnTopElem :: (SynthSignal -> Synth SynthSignal) -> Synth ()
--synthOnTopElem f = use synthElements >>= \ case
--  []         -> return ()
--  elem:elems -> do
--    comps <- forEachFDComponent (elem ^. synthElemSignal . fdSignalComponents) f
--    synthElements .= (elem & synthElemSignal . fdSignalComponents .~ comps) : elems

--    synthPushNewElem, synthOnTopElem,

----------------------------------------------------------------------------------------------------

---- | A data type for declaring the shape of an 'FDSignal'.
--data FDSignalShape
--  = FDSignalShape
--    { theFDShapeBase       :: Frequency
--      -- ^ The base frequency, which splits the audible spectrum into "lower" and "upper" bands.
--    , theFDShapeLowerLimit :: Frequency
--      -- ^ The lowest audible frequency of the shape
--    , theFDShapeLoEnvelope :: Envelope
--      -- ^ The envelope that defines the shape of the 'FDSignal' to be produced by this
--      -- structure. The 'Procedgen.Types.Envelope' function is expected to be defined only on the
--      -- range between and including 0.0 and 1.0, and will be transformed to fit in the region of
--      -- frequencies between 'theFDShapeLowerLimit' and 'theFDShapeBase'.
--    , theFDShapeUpperLimit :: Frequency
--      -- ^ Like 'theFDShapeLowerLimit' but defines a point above 'theFDShapeBase' frequency.
--    , theFDShapeHiEnvelope :: Envelope
--      -- ^ Like 'theFDShapeLoEnvelope' but defines an envelope applied to the region of frequencies
--      -- between 'theFDShapeBase' and 'theFDShapeUpperLimit'.
--    }
--
---- | A default 'FDSignalShape' which always evaluates to 1.0.
--fdSignalShapeFlat :: Amplitude -> FDSignalShape
--fdSignalShapeFlat a = FDSignalShape
--  { theFDShapeBase       = 256.0
--  , theFDShapeLowerLimit = 15.0
--  , theFDShapeLoEnvelope = const $ const a
--  , theFDShapeUpperLimit = nyquist
--  , theFDShapeHiEnvelope = const $ const a
--  }
--
--fdShapeBase :: Lens' FDSignalShape Frequency
--fdShapeBase = lens theFDShapeBase $ \ a b -> a{ theFDShapeBase = b }
--
--fdShapeLowerLimit :: Lens' FDSignalShape Frequency
--fdShapeLowerLimit = lens theFDShapeLowerLimit $ \ a b -> a{ theFDShapeLowerLimit = b }
--
--fdShapeUpperLimit :: Lens' FDSignalShape Frequency
--fdShapeUpperLimit = lens theFDShapeUpperLimit $ \ a b -> a{ theFDShapeUpperLimit = b }
--
--fdShapeLoEnvelope :: Lens' FDSignalShape Envelope
--fdShapeLoEnvelope = lens theFDShapeLoEnvelope $ \ a b -> a{ theFDShapeLoEnvelope = b }
--
--fdShapeHiEnvelope :: Lens' FDSignalShape Envelope
--fdShapeHiEnvelope = lens theFDShapeHiEnvelope $ \ a b -> a{ theFDShapeHiEnvelope = b }
--
---- | Given a 'Frequency', return an amplitude for that frequency that fits the 'FDSignalShape'.
--applyFDSignalShape :: FDSignalShape -> Frequency -> Amplitude
--applyFDSignalShape sh freq =
--  if sh ^. fdShapeLowerLimit <= freq || freq <= sh ^. fdShapeUpperLimit then 0.0 else
--    case compare freq $ sh ^. fdShapeBase of
--      EQ -> 1.0
--      LT -> freq & (sh ^. fdShapeLoEnvelope)
--        (TimeWindow{ timeStart = sh ^. fdShapeLowerLimit, timeEnd = sh ^. fdShapeBase })
--      GT -> freq & (sh ^. fdShapeHiEnvelope)
--        (TimeWindow{ timeStart = sh ^. fdShapeBase, timeEnd = sh ^. fdShapeUpperLimit })
--
