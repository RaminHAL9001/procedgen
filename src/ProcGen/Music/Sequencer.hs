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
  ( -- * The Track Data Type
    Track(..), trackTime, trackSampleCount, newTrack, writeTrackFile, readTrackFile,
    -- * Sequencer Evaluation
    Sequencer, SequencerState(..), PlayToTrack(..),
    newSequencer, runSequencer, liftSynth, 
    SoundSet(..), Sound(..), HasSoundSet(..), addSound, deleteSound,
    -- * Defining Drum Sets
    DrumID(..), DrumKit, drumSounds, addDrum, getDrum,
    -- * Defining Tonal Insruments
    InstrumentID(..), ToneID(..), ToneInstrument, ToneKeyIndicies(..), ToneTagSet, ToneTag(..),
    addInstrument, toneInstrument, toneSounds, addTone, getTone,
  ) where

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Music.Composition
import           ProcGen.Music.KeyFreq88
import           ProcGen.Music.Synth

import           Control.Lens
import           Control.Monad.Random
import           Control.Monad.State

import           Data.List                  (nub, sort)
import qualified Data.Map                    as Map
import           Data.Semigroup
import           Data.String
import qualified Data.Text                   as Strict
import qualified Data.Vector                 as Boxed
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as MUnboxed
--import qualified Data.Vector.Mutable         as MBoxed
import           Data.Word

----------------------------------------------------------------------------------------------------

newtype Track = Track (MUnboxed.IOVector Sample)

trackTime :: Track -> Duration
trackTime = sampleCountDuration . trackSampleCount

trackSampleCount :: Track -> SampleCount
trackSampleCount (Track vec) = MUnboxed.length vec

newTrack :: MonadIO m => Duration -> m Track
newTrack = liftM Track . liftIO . MUnboxed.new . durationSampleCount

writeTrackFile :: FilePath -> Track -> IO ()
writeTrackFile = error "TODO: ProcGen.Music.Sequencer.writeTrackFile"

-- | Must be a .WAV file, 44100 hz 16 bit signed little endian single channel.
readTrackFile :: FilePath -> Track -> IO ()
readTrackFile = error "TODO: ProcGen.Music.Sequencer.readTrackFile"

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
    , theSequencerDrumKit     = DrumKit Map.empty
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

addTone :: InstrumentID -> [ToneTag] -> ToneKeyIndicies -> Sound -> Sequencer ToneID
addTone instrm tags key sound = do
  let toneID    = ToneID key $ soundTagSet tags
  let newInstrm = uncurry toneInstrument $ minMaxKeyIndex key
  sequencerInstrument %=
    Map.alter (Just . addToneToInstrument toneID sound . maybe newInstrm id) instrm
  return toneID

getTone :: InstrumentID -> ToneID -> Sequencer (Maybe Sound)
getTone instrm toneID =
  (join . fmap (view $ toneSounds toneID) . Map.lookup instrm) <$> use sequencerInstrument >>=
  maybe (pure Nothing) chooseSound

----------------------------------------------------------------------------------------------------

-- | This type class defines a 'playToTrack' function which can be instantiated by any data type
-- that can render a sound into a buffer.
class PlayToTrack a where
  playToTrack :: Track -> a -> Sequencer ()

----------------------------------------------------------------------------------------------------

-- | A 'DrumID' identifies elements of a drum set. "Drums" are really just arbitrary sound effects
-- that are not categorized by tone. If your sounds can be categorized by tone, consider associating
-- them with a 'ToneID' instead of a 'DrumID'.
newtype DrumID = DrumID Strict.Text
  deriving (Eq, Ord, Show)

instance IsString DrumID where { fromString = DrumID . Strict.pack; }

----------------------------------------------------------------------------------------------------

newtype InstrumentID = InstrumentID Strict.Text
  deriving (Eq, Ord, Show)

instance IsString InstrumentID where { fromString = InstrumentID . Strict.pack; }

----------------------------------------------------------------------------------------------------

-- | Acts as a key that can be looked up in a map/dictionary, and serves as a unique descriptor of
-- the content of a buffer or a cluster of buffers. If there are more than one buffer associated
-- with a sound, when the sound is selected, one of the buffers is chosen at random.
--
-- This randomizing feature is designed to make an instrument sound more natural. For example, you
-- may have 3 versions of a violin sound playing the note A4 with
-- 'ProcGen.Music.Composition.MezzoForte' strength, perhaps one which is a slightly squeaky or
-- scratchy sound. Every time the sound A4 mezzo forte is selected, one of those three are seleted
-- from a 'Sequencer' using 'getSound'. The squeaky/scratchy sound can be weighted such that it is
-- randomly selected less often.
data ToneID = ToneID !ToneKeyIndicies !ToneTagSet
  deriving (Eq, Ord, Show)

-- | Identify a sound by it's tone, or it's tone-transition (slide or cross-faded). This is not used
-- for drum kits.
data ToneKeyIndicies
  = KeyTone    !KeyIndex
  | SlideTone  !KeyIndex !KeyIndex
  | CrossFade  !KeyIndex !KeyIndex
  deriving (Eq, Ord, Show)

-- | Additional tags for a sound. A 'ToneID' has any number of these additional tags.
data ToneTag = Vibrato | Rolled | Muffled | Flubbed | Scratched
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype ToneTagSet = ToneTagSet (Unboxed.Vector Word8)
  deriving (Eq, Ord)

instance Show ToneTagSet where
  show (ToneTagSet vec) = show $ fromEnum . (fromIntegral :: Word8 -> Int) <$> Unboxed.toList vec

soundTagSet :: [ToneTag] -> ToneTagSet
soundTagSet = ToneTagSet . Unboxed.fromList . fmap (fromIntegral . fromEnum) . nub

minMaxKeyIndex :: ToneKeyIndicies -> (KeyIndex, KeyIndex)
minMaxKeyIndex = \ case
  KeyTone   a   -> (a, a)
  SlideTone a b -> (min a b, max a b)
  CrossFade a b -> (min a b, max a b)

----------------------------------------------------------------------------------------------------

-- | A buffer contains meta-information about a 'TDSignal' constructed by a 'ProcGen.Music.Synth'.
data Sound
  = Sound
    { soundFromFild     :: !Strict.Text -- ^ might be null
    , soundFromFD       :: !(Maybe FDSignal)
    , soundLikelyChoice :: !Percentage
      -- ^ When randomly choosing this sound from a list of sounds, how likely this sound will be
      -- picked can be weighted by this value.
    , soundTDSignal     :: !TDSignal
    }
  deriving Eq

instance Ord Sound where
  compare a b = soundLikelyChoice b `compare` soundLikelyChoice a

data ToneInstrument
  = ToneInstrument
    { toneLowest   :: !KeyIndex
    , toneHighest  :: !KeyIndex
    , theToneTable :: !(Map.Map ToneID SoundSet)
    }

instance Semigroup ToneInstrument where
  a <> b = ToneInstrument
    { toneLowest   = toneLowest  a `min` toneLowest  b
    , toneHighest  = toneHighest a `max` toneHighest b
    , theToneTable = Map.unionWith mappend (theToneTable b) (theToneTable a)
    }

-- | Construct a new 'ToneInstrument'
toneInstrument :: KeyIndex -> KeyIndex -> ToneInstrument
toneInstrument lo hi = ToneInstrument
  { toneLowest   = min lo hi
  , toneHighest  = max lo hi
  , theToneTable = Map.empty
  }

toneTable :: Lens' ToneInstrument (Map.Map ToneID SoundSet)
toneTable = lens theToneTable $ \ a b -> a{ theToneTable = b }

addToneToInstrument :: ToneID -> Sound -> ToneInstrument -> ToneInstrument
addToneToInstrument toneID sound = toneTable %~ 
  Map.insertWith (<>) toneID (SoundSet $ Boxed.singleton sound)

----------------------------------------------------------------------------------------------------

newtype DrumKit = DrumKit { theDrumTable :: Map.Map DrumID SoundSet }

drumTable :: Lens' DrumKit (Map.Map DrumID SoundSet)
drumTable = lens theDrumTable $ \ a b -> a{ theDrumTable = b }

addDrumToKit :: DrumID -> Sound -> DrumKit -> DrumKit
addDrumToKit key val = drumTable %~ Map.insertWith (<>) key (SoundSet $ Boxed.singleton val)

----------------------------------------------------------------------------------------------------

newtype SoundSet = SoundSet { soundSetVector :: Boxed.Vector Sound }

instance Semigroup SoundSet where
  (SoundSet a) <> (SoundSet b) = SoundSet $ Boxed.fromList $ sort $ Boxed.toList a ++ Boxed.toList b

instance Monoid SoundSet where { mempty = SoundSet mempty; mappend = (<>); }

class HasSoundSet set idx | set -> idx where
  soundSet :: idx -> Lens' set (Maybe SoundSet)

instance HasSoundSet ToneInstrument ToneID where { soundSet = toneSounds; }
instance HasSoundSet DrumKit        DrumID where { soundSet = drumSounds; }

-- | Like 'sounds' but specific to the 'ToneID' and 'ToneInstrument' types.
toneSounds :: ToneID -> Lens' ToneInstrument (Maybe SoundSet)
toneSounds i = lens (Map.lookup i . theToneTable) $ \ tone table ->
  tone{ theToneTable = Map.alter (const table) i $ theToneTable tone }

drumSounds :: DrumID -> Lens' DrumKit (Maybe SoundSet)
drumSounds i = lens (Map.lookup i . theDrumTable) $ \ drum table ->
  drum{ theDrumTable = Map.alter (const table) i $ theDrumTable drum }

-- | Prepend 'Sound' data to the front of the 'Boxed.Vector'.
addSound :: Sound -> SoundSet -> SoundSet
addSound info = SoundSet . Boxed.fromList . sort . (info :) . Boxed.toList . soundSetVector

-- | Remove 'Sound' data from some index of the 'Boxed.Vector'.
deleteSound :: Int -> SoundSet -> SoundSet
deleteSound i (SoundSet vec) = SoundSet $
  if Boxed.length vec <= i || i < 0 then vec else Boxed.fromList $
    (vec Boxed.!) <$> ([0 .. i - 1] ++ [i + 1 .. Boxed.length vec - 1])

-- | Randomly select a 'Sound' from the 'Boxed.Vector'. 
chooseSound :: forall m . (Monad m, MonadRandom m) => SoundSet -> m (Maybe Sound)
chooseSound (SoundSet vec) = do
  let scale = sum $ soundLikelyChoice <$> Boxed.toList vec
  prob <- liftM (* scale) getRandom :: m Float
  -- This loop checks each element in the list in order, but it is expected that these vectors will
  -- be very small, usually around 4 elements or less.
  let loop n =  \ case
        []       -> return Nothing
        s:sounds -> seq n $! if n >= prob then return $ Just s else
          loop (n + soundLikelyChoice s) sounds
  loop 0 $ Boxed.toList vec
