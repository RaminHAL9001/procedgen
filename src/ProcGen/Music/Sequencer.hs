-- | The sequencer is the component which translates a musical
-- 'ProcGen.Music.Composition.Composition' into an audio file. This module ties together the
-- functionality of "ProcGen.Music.Synth" and "ProcGen.Music.Composition". The sequencer input
-- language has a format somewhat similar to MIDI. There are features to maintain different versions
-- of the same note repetition produces slightly different sounds for each note played, sounding
-- slightly more natural.
--
-- A sequencer's job is to take instructions which call the "ProcGen.Music.Synth" synthesizer to
-- generate time-domain ('ProcGen.Music.Synth.TDSignal') buffers, and then mix these buffers to a
-- larger buffer, perhaps also applying post-processing effects (in future versions of this
-- program).
module ProcGen.Music.Sequencer
  ( -- * Shaping Signals
    test,
    ShapedSignal(..), basicShapedSignal, squareShapedSignal,
    shapedTDSignal, shapedSound, setShapedSignalDuration, setShapedSoundDuration,
    shapedSignal, shapeInitTime, shapeDuration,
    shapeAttackDuration, shapeDecayDuration, shapeAttackEnvelope, shapeDecayEnvelope,
    -- * The Track Data Type
    Track(..), Target, Source,
    trackTime, trackSampleCount, newTrack, writeTrackFile, readTrackFile,
    -- * Sequencer Evaluation
    musicToFile,
    Sequencer, SequencerState(..), PlayToTrack(..),
    newSequencer, runSequencer, liftSynth,
    addDrum, getDrum, addInstrument, addTone, getTone,
  ) where

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Buffer
import           ProcGen.Music.AudioFont
import           ProcGen.Music.AudioSignal
import           ProcGen.Music.Composition
import           ProcGen.Music.KeyFreq88
import           ProcGen.Music.Synth
import           ProcGen.Music.WaveFile

import           Control.Lens

import qualified Data.Map                    as Map
import           Data.Semigroup
import qualified Data.Vector.Unboxed.Mutable as Mutable
import qualified Data.Vector.Unboxed         as Unboxed

----------------------------------------------------------------------------------------------------

newtype Track = Track (Mutable.IOVector Sample)

-- | When copying from one 'Track' to another, this function type denotes which 'Track' is the
-- target.
type Target a = a

-- | When copying from one 'Track' to another, this function type denotes which 'Track' is the
-- source.
type Source a = a

trackTime :: Track -> Duration
trackTime = sampleCountDuration . trackSampleCount

trackSampleCount :: Track -> SampleCount
trackSampleCount (Track vec) = Mutable.length vec

newTrack :: MonadIO m => Duration -> m Track
newTrack = liftM Track . liftIO . Mutable.new . durationSampleCount

writeTrackFile :: FilePath -> Track -> IO ()
writeTrackFile path (Track vec) = putRiffWaveFormatIO path vec

-- | Must be a @.WAV@ file, 44100 hz 16 bit signed little endian single channel.
readTrackFile :: FilePath -> IO Track
readTrackFile = fmap Track . getRiffWaveFormatIO

musicToFile
  :: FilePath       -- ^ the sound file to be created.
  -> TFRandSeed     -- ^ a psuedo-random number generator seed number.
  -> Sequencer ()   -- ^ initialize the sequencer.
  -> Composition () -- ^ compose the music.
  -> IO ()
musicToFile path seed seqinit f = do
  (((), comp), _) <- runCompositionTFGen f (tfGen seed) emptyComposition
  let roles = getPlayedRoles comp
  case timeWindow roles of
    Nothing -> return ()
    Just tw@(TimeWindow{timeStart=t0,timeEnd=t1}) -> do
      track <- newTrack (2.0 + twDuration tw)
      seqst <- newSequencer
      let wholeShape sig = basicShapedSignal sig (t1 - t0 + 2) & shapeInitTime .~ 0.5
      ((), _seqst) <- flip runSequencer seqst $
        seqinit >> mapM_ (playToTrack track 0.5 . wholeShape) roles
      writeTrackFile path track

-- | TODO: delete this function, it shouldn't be here in the release version.
test :: IO TDView
test = do
  let filepath = "./example.wav" :: FilePath
  musicToFile filepath 0 (pure ()) exampleComposition
  putStrLn $ "Created " ++ show filepath
  (`tdView` (keyboard88 $ keyIndex 39)) <$> readTDSignalFile filepath

----------------------------------------------------------------------------------------------------

-- | When applying a signal of some type to a 'Track', the signal should be shaped, meaning the
-- duration and envelope of the attack and decay must be defined. The total time the sound is
-- audible will be equal to the sum of the three parameters: 'shapeAttackDuration', 'shapeDuration',
-- and 'shapeDecayDuration'.
data ShapedSignal signal
  = ShapedSignal
    { theShapeInitTime       :: !Moment
    , theShapeDuration       :: !Duration
    , theShapeAttackDuration :: !Duration
    , theShapeDecayDuration  :: !Duration
    , theShapedSignal        :: signal
    , theShapeAttackEnvelope :: Envelope
    , theShapeDecayEnvelope  :: Envelope
    }
  deriving Functor

-- | A default 'ShapedSignal' constructor.
basicShapedSignal :: signal -> Duration -> ShapedSignal signal
basicShapedSignal sig dt = ShapedSignal
  { theShapedSignal        = sig
  , theShapeInitTime       = 0
  , theShapeDuration       = dt - 2/15
  , theShapeAttackDuration = 1/15
  , theShapeDecayDuration  = 1/15
  , theShapeAttackEnvelope = sigmoid
  , theShapeDecayEnvelope  = sigmoid
  }

-- | A 'ShapedSignal' with no attack envelope and no decay envelope. Use this to construct a
-- 'ShapedSignal' that applies no shape at all to the contained @signal@.
squareShapedSignal :: signal -> Duration -> ShapedSignal signal
squareShapedSignal sig dt = ShapedSignal
  { theShapedSignal        = sig
  , theShapeInitTime       = 0
  , theShapeDuration       = dt
  , theShapeAttackDuration = 0
  , theShapeDecayDuration  = 0
  , theShapeAttackEnvelope = const . const 1.0
  , theShapeDecayEnvelope  = const . const 1.0
  }

-- | Construct a 'squareShapedSignal' from a 'ProcGen.Music.Synth.TDSignal' that encompases the
-- entire 'ProcGen.Music.Synth.TDSignal'.
shapedTDSignal :: TDSignal -> ShapedSignal TDSignal
shapedTDSignal sig = squareShapedSignal sig $ indexToTime $ Unboxed.length (tdSamples sig) - 1

shapedSound :: Sound -> ShapedSignal Sound
shapedSound snd = squareShapedSignal snd (tdDuration $ soundTDSignal snd) &~
  case soundRenderedFromFDSignal snd of
    Nothing -> return ()
    Just fd -> do
      let fadetime = 2.0 / theFDBaseFreq fd
      shapeAttackDuration .= fadetime
      shapeDecayDuration  .= fadetime

-- | Set the 'shapeDuration' of the 'ShapedSignal' to some other value, and also set the
-- 'shapeDecayDuration' and the 'shapeDecayEnvelope'
setShapedSignalDuration
  :: Duration -- ^ will set the 'shapeDuration'
  -> Duration -- ^ will set the 'shapeDecayDuration'
  -> ShapedSignal signal
  -> ShapedSignal signal
setShapedSignalDuration td decay snd = snd &~ do
  shapeDuration      .= td
  shapeDecayDuration .= decay
  shapeDecayEnvelope .= sigmoid

-- | Like 'setShapedSignalDuration', but automatically passes the 'shapeDecayDuration' parameter
-- based on the 'fdBaseFreq' base frequency of the 'TDSignal' stored within this sound signal.
setShapedSoundDuration :: Duration -> ShapedSignal Sound -> ShapedSignal Sound
setShapedSoundDuration td snd = fmap (const $ theShapedSignal snd) $
  setShapedSignalDuration td
    (maybe (1/15) theFDBaseFreq $ soundRenderedFromFDSignal $ theShapedSignal snd)
    (soundTDSignal <$> snd)

-- | The @signal@ itself.
shapedSignal :: Lens' (ShapedSignal signal) signal
shapedSignal = lens theShapedSignal $ \ a b -> a{ theShapedSignal = b }

-- | The start time within the @signal@ that this shaper should begin copying to the 'Track'.
shapeInitTime :: Lens' (ShapedSignal signal) Moment
shapeInitTime = lens theShapeInitTime $ \ a b -> a{ theShapeInitTime = b }

-- | The time duration within the @signal@ that this shaper should begin copying to the 'Track'.
shapeDuration :: Lens' (ShapedSignal signal) Duration
shapeDuration = lens theShapeDuration $ \ a b -> a{ theShapeDuration = b }

-- | How much time should be spent applying the 'shapeAttackEnvelope'.
shapeAttackDuration :: Lens' (ShapedSignal signal) Duration
shapeAttackDuration = lens theShapeAttackDuration $ \ a b -> a{ theShapeAttackDuration = b }

-- | How much time should be spent applying the 'shapeDecayEnvelope'.
shapeDecayDuration :: Lens' (ShapedSignal signal) Duration
shapeDecayDuration = lens theShapeDecayDuration $ \ a b -> a{ theShapeDecayDuration = b }

-- | When copying the @signal@ to the 'Track', the @signal@ will be "faded-in" for a brief moment to
-- prevent clicking that tends to occur when there is an abrupt change in the @signal@ at a certain
-- point in time.
shapeAttackEnvelope :: Lens' (ShapedSignal signal) Envelope
shapeAttackEnvelope = lens theShapeAttackEnvelope $ \ a b -> a{ theShapeAttackEnvelope = b }

-- | How much time should be spent applying the 'shapeDecayEnvelope'.
shapeDecayEnvelope :: Lens' (ShapedSignal signal) Envelope
shapeDecayEnvelope = lens theShapeDecayEnvelope $ \ a b -> a{ theShapeDecayEnvelope = b }

----------------------------------------------------------------------------------------------------

-- | This type class defines a 'playToTrack' function which can be instantiated by any data type
-- that can render a sound into a buffer. Sounds written by 'playToTrack' should overwrite whatever
-- exists in the buffer, no mixing of signals should occur in this step.
class PlayToTrack signal where
  playToTrack :: Target Track -> Target Moment -> Source (ShapedSignal signal) -> Sequencer ()

instance PlayToTrack (PlayedRole PlayedTone) where
  playToTrack track t0 signal = do
    let role = theShapedSignal signal
    let getNotes f = forM_ (listNoteSequence $ thePlayedRoleSequence role) $ \ (t, notes) ->
          forM_ (notes >>= getTiedNotes) $ f $ t0 + t
    gets (Map.lookup (thePlayedRoleInstrument role) . theSequencerInstruments) >>= \ case
      Nothing     -> getNotes $ playSinusoidToTrack track
      Just instrm -> getNotes $ \ t note ->
        sequencerInstrumentNote instrm (playedNoteValue note) >>=
        maybe (return ()) (playToTrack track t . flip basicShapedSignal (playedDuration note))

instance PlayToTrack Sound where
  playToTrack track t0 = playToTrack track t0 . fmap soundTDSignal

instance PlayToTrack [Sound] where
  playToTrack track t0 signal = forM_ (theShapedSignal signal) $ \ sound ->
    playToTrack track t0 signal{ theShapedSignal = sound }

instance PlayToTrack Track where
  playToTrack = let vec (Track v) = v in
    copyVecToTrack (Mutable.length . vec) (Mutable.read . vec)

instance PlayToTrack TDSignal where
  playToTrack track t0 = playToTrack track t0 . fmap tdSamples

instance PlayToTrack (Unboxed.Vector Sample) where
  playToTrack = copyVecToTrack Unboxed.length (\ vec -> pure . (vec Unboxed.!))

copyVecToTrack
  :: (vec -> Int) -- ^ get the size
  -> (vec -> Int -> IO Sample) -- ^ read the vector
  -> Target Track -> Target Moment -> ShapedSignal vec -> Sequencer ()
copyVecToTrack length read (Track vecT) t0 shapesig = do
  let vecS   = shapesig ^. shapedSignal
  let lenS   = length vecS
  let topS   = sampleCountDuration lenS
  let s0     = shapesig ^. shapeInitTime
  let att'   = max 0 $ shapesig ^. shapeAttackDuration
  let dect'  = max 0 $ shapesig ^. shapeDecayDuration
  let dt     = max 0 $ shapesig ^. shapeDuration
  let edges  = att' + dect'
  let escale = if att' >= 0 && dect' >= 0 && dt < edges then dt / edges else 1.0
  let (att, dect) = (att' * escale, dect' * escale)
  let time t0 dt = TimeWindow
        { timeStart = t0
        , timeEnd   = t0 + min (s0 + dt) topS - s0
        }
  let attackWin = time t0 att
  let playWin   = time (timeEnd attackWin) dt
  let decayWin  = time (timeEnd playWin) dect
  let idx       = fmap durationSampleCount
  let f i e     = (+ e) <$> read vecS i
  when (dt > 1/32) $ liftIO $ do
    mapBuffer vecT (idx attackWin) f
    mapBuffer vecT (idx playWin  ) f
    mapBuffer vecT (idx decayWin ) f

sequencerInstrumentNote :: ToneInstrument -> ToneID -> Sequencer (Maybe Sound)
sequencerInstrumentNote instr tone = case Map.lookup tone $  instr ^. toneTable of
  Nothing       -> return Nothing
  Just soundset -> chooseSound soundset

playSinusoidToTrack :: Target Track -> Target Moment -> PlayedTone -> Sequencer ()
playSinusoidToTrack (Track vecT) t0 = \ case
  RestNote -> return ()
  note     -> do
    let (NoteID idx _tag) = playedNoteValue note
    let dt = playedDuration note
    let (idxA, idxB) = case idx of
          NoteKey  a   -> (a, a)
          NoteTied a b -> (a, b)
    let (lenA, lenB) = (noteWeight idxA, noteWeight idxB)
    let key len = take len . cycle . fmap keyboard88 . noteKeyIndicies
    forM_ (zip (key lenA idxA) (key lenB idxB)) $ \ (a, b) -> do
      let i0   = durationSampleCount t0
      let tone = if a == b then const a else \ t -> a + (a - b) *
            sigmoid TimeWindow{ timeStart = 0, timeEnd = dt } t
      let fade_0_to_dt = fadeInOut 0 (2 / a) (dt - 2 / b) dt
      phase <- onRandFloat (* (2 * pi))
      liftIO $ mapBuffer vecT
        (TimeWindow{ timeStart = i0, timeEnd = i0 + durationSampleCount dt }) $ \ i e -> do
          let t = indexToTime $ i - i0
          pure $ e + sinePulse (tone t) phase t * fade_0_to_dt t

----------------------------------------------------------------------------------------------------

newtype Sequencer a = Sequencer (StateT SequencerState IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState SequencerState Sequencer where { state = Sequencer . state; }

instance MonadRandom Sequencer where
  getRandomR  = liftTFRand . getRandomR
  getRandom   = liftTFRand getRandom
  getRandomRs = liftTFRand . getRandomRs
  getRandoms  = liftTFRand getRandoms

data SequencerState
  = SequencerState
    { theSequencerSynth        :: !SynthState
    , theSequencerDrumKit      :: !DrumKit
    , theSequencerInstruments  :: !(Map.Map InstrumentID ToneInstrument)
    }

sequencerSynth :: Lens' SequencerState SynthState
sequencerSynth = lens theSequencerSynth $ \ a b -> a{ theSequencerSynth = b }

sequencerDrumKit :: Lens' SequencerState DrumKit
sequencerDrumKit = lens theSequencerDrumKit $ \ a b -> a{ theSequencerDrumKit = b }

sequencerInstrument :: Lens' SequencerState (Map.Map InstrumentID ToneInstrument)
sequencerInstrument = lens theSequencerInstruments $ \ a b -> a{ theSequencerInstruments = b }

runSequencer :: Sequencer a -> SequencerState -> IO (a, SequencerState)
runSequencer (Sequencer f) = runStateT f

newSequencer :: IO SequencerState
newSequencer = do
  synth <- initSynth
  return SequencerState
    { theSequencerSynth       = synth
    , theSequencerDrumKit     = mempty
    , theSequencerInstruments = Map.empty
    }

-- | Evaluate a function of type 'ProcGen.Music.Synth.Synth' within a function of type
-- 'SequencerState'.
liftSynth :: Synth a -> Sequencer a
liftSynth f = do
  (a, synth) <- use sequencerSynth >>= liftIO . runSynth f
  sequencerSynth .= synth
  return a

-- | Evaluate a pure 'ProcGen.Arbitrary.TFRand' function within a 'ProcGen.Music.Synth.Synth'
-- function.
liftTFRand :: TFRand a -> Sequencer a
liftTFRand f = do
  (a, gen) <- runTFRand f <$> use (sequencerSynth . synthTFGen)
  sequencerSynth . synthTFGen .= gen
  return a

-- | Associate a 'DrumID' with a 'Sound', or append the 'Sound' to the 'SoundSet' if the 'DrumID'
-- already has one or more 'Sound's associated with it.
addDrum :: DrumID -> Sound -> Sequencer ()
addDrum drum sound = sequencerDrumKit %= addDrumToKit drum sound

-- | Select a sound for a given 'DrumID'. If more than one 'Sound' has been added to the same
-- 'DrumID', one of the 'Sound's will be selected at random.
getDrum :: DrumID -> Sequencer (Maybe Sound)
getDrum key = use (sequencerDrumKit . drumSounds key) >>= maybe (pure Nothing) chooseSound

-- | Create a new 'ToneInstrument' for use within this 'Sequencer', or update an existing
-- instrument.
addInstrument :: InstrumentID -> KeyIndex -> KeyIndex -> Sequencer InstrumentID
addInstrument instrm lo hi = do
  sequencerInstrument %= Map.insertWith (<>) instrm (toneInstrument lo hi)
  return instrm

addTone :: InstrumentID -> [PlayNoteTag] -> ToneKeyIndicies -> Sound -> Sequencer ToneID
addTone instrm tags key sound = do
  let toneID    = NoteID key $ playNoteTagSet tags
  let newInstrm = uncurry toneInstrument $ minMaxKeyIndex key
  sequencerInstrument %=
    Map.alter (Just . addToneToInstrument toneID sound . maybe newInstrm id) instrm
  return toneID

getTone :: InstrumentID -> ToneID -> Sequencer (Maybe Sound)
getTone instrm toneID =
  (join . fmap (view $ toneSounds toneID) . Map.lookup instrm) <$> use sequencerInstrument >>=
  maybe (pure Nothing) chooseSound

