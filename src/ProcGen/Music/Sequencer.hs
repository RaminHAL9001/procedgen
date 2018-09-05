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
    Sequencer, SequencerState, PlayToTrack(..),
    runSequencer, evalSynth, 
    BufferSet(..), BufferInfo(..), addBuffer, deleteBuffer,
    -- * Defining Drum Sets
    DrumID(..), DrumInstrument, drumBuffers, newDrum, getDrum,
    -- * Defining Tonal Insruments
    ToneID(..), ToneInstrument, ToneKeyIndicies(..), ToneTagSet, ToneTag(..), toneBuffers,
    newTone, getTone,
  ) where

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Music.Composition
import           ProcGen.Music.KeyFreq88
import           ProcGen.Music.Synth

import           Control.Lens
import           Control.Monad.State

import           Data.List                  (nub)
import qualified Data.Map                    as Map
import           Data.String
import qualified Data.Text                   as Strict
import qualified Data.Vector                 as Boxed
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as MUnboxed
import qualified Data.Vector.Mutable         as MBoxed
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

data SequencerState
  = SequencerState
    { theSequencerGen         :: !TFGen
    , theSequencerDrumKit     :: !DrumInstrument
    , theSequencerInstruments :: !(Map.Map Strict.Text ToneInstrument)
    }

sequencerGen :: Lens' SequencerState TFGen
sequencerGen = lens theSequencerGen $ \ a b -> a{ theSequencerGen = b }

sequencerDrumKit :: Lens' SequencerState DrumInstrument
sequencerDrumKit = lens theSequencerDrumKit $ \ a b -> a{ theSequencerDrumKit = b }

sequencerInstrument :: Lens' SequencerState (Map.Map Strict.Text ToneInstrument)
sequencerInstrument = lens theSequencerInstruments $ \ a b -> a{ theSequencerInstruments = b }

----------------------------------------------------------------------------------------------------

-- | A 'DrumID' identifies elements of a drum set. "Drums" are really just arbitrary sound effects
-- that are not categorized by tone. If your sounds can be categorized by tone, consider associating
-- them with a 'ToneID' instead of a 'DrumID'.
newtype DrumID = DrumID Strict.Text
  deriving (Eq, Ord, Show)

instance IsString DrumID where { fromString = DrumID . Strict.pack; }

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
  = KeyTone      !KeyIndex
  | SlideTone    !KeyIndex !KeyIndex
  | CrossFaded   !KeyIndex !KeyIndex
  deriving (Eq, Ord, Show)

-- | Additional tags for a sound. A 'ToneID' has any number of these additional tags.
data ToneTag = Vibrato | Rolled | Muffled | Flubbed | Scratched
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype ToneTagSet = ToneTagSet (Unboxed.Vector Word8)
  deriving (Eq, Ord)

soundTagSet :: [ToneTag] -> ToneTagSet
soundTagSet = ToneTagSet . Unboxed.fromList . fmap (fromIntegral . fromEnum) . nub

instance Show ToneTagSet where
  show (ToneTagSet vec) = show $ fromEnum . fromIntegral <$> Unboxed.toList vec

----------------------------------------------------------------------------------------------------

data BufferInfo
  = BufferInfo
    { bufferedFromFile :: !Strict.Text -- ^ might be null
    , bufferedFromFD   :: !(Maybe FDSignal)
    , bufferedTDSignal :: !TDSignal
    }
  deriving Eq

data ToneInstrument
  = ToneInstrument
    { theToneLabel   :: !Strict.Text
    , theToneLowest  :: !KeyIndex
    , theToneHighest :: !KeyIndex
    , theToneTable   :: !(Map.Map ToneID (Boxed.Vector BufferInfo))
    }

data DrumInstrument
  = DrumInstrument
    { theDrumLabel :: !Strict.Text
    , theDrumTable :: !(Map.Map DrumID (Boxed.Vector BufferInfo))
    }

toneLabel :: Lens' ToneInstrument Strict.Text
toneLabel = lens theToneLabel $ \ a b -> a{ theToneLabel = b }

toneLowest :: Lens' ToneInstrument KeyIndex
toneLowest = lens theToneLowest $ \ a b -> a{ theToneLowest = b }

toneHighes :: Lens' ToneInstrument KeyIndex
toneHighes = lens theToneHighest $ \ a b -> a{ theToneHighest = b }

toneTable :: Lens' ToneInstrument (Map.Map ToneID (Boxed.Vector BufferInfo))
toneTable = lens theToneTable $ \ a b -> a{ theToneTable = b }

drumLabel :: Lens' DrumInstrument Strict.Text
drumLabel = lens theDrumLabel $ \ a b -> a{ theDrumLabel = b }

drumTable :: Lens' DrumInstrument (Map.Map DrumID (Boxed.Vector BufferInfo))
drumTable = lens theDrumTable $ \ a b -> a{ theDrumTable = b }

----------------------------------------------------------------------------------------------------

class BufferSet set idx | set -> idx where
  buffers :: idx -> Lens' set (Maybe (Boxed.Vector BufferInfo))
  --getSound :: set -> idx -> Sequencer BufferInfo -- TODO

instance BufferSet ToneInstrument ToneID where { buffers = toneBuffers; }
instance BufferSet DrumInstrument DrumID where { buffers = drumBuffers; }

-- | Like 'buffers' but specific to the 'ToneID' and 'ToneInstrument' types.
toneBuffers :: ToneID -> Lens' ToneInstrument (Maybe (Boxed.Vector BufferInfo))
toneBuffers i = lens (Map.lookup i . theToneTable) $ \ tone table ->
  tone{ theToneTable = Map.alter (const table) i $ theToneTable tone }

drumBuffers :: DrumID -> Lens' DrumInstrument (Maybe (Boxed.Vector BufferInfo))
drumBuffers i = lens (Map.lookup i . theDrumTable) $ \ drum table ->
  drum{ theDrumTable = Map.alter (const table) i $ theDrumTable drum }

-- | Prepend 'BufferInfo' data to the front of the 'Boxed.Vector'.
addBuffer :: BufferInfo -> Boxed.Vector BufferInfo -> Boxed.Vector BufferInfo
addBuffer info = Boxed.fromList . (info :) . Boxed.toList

-- | Remove 'BufferInfo' data from some index of the 'Boxed.Vector'.
deleteBuffer :: Int -> Boxed.Vector BufferInfo -> Boxed.Vector BufferInfo
deleteBuffer i vec = if Boxed.length vec <= i || i < 0 then vec else Boxed.fromList $
  (vec Boxed.!) <$> ([0 .. i - 1] ++ [i + 1 .. Boxed.length vec - 1])
