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
  ( -- * Sequencer Evaluation
    Sequencer, SequencerState, runSequencer, evalSynth, newTrack, deleteTrack, playToTrack,
    BufferSet(..), BufferInfo(..), addBuffer, deleteBuffer,
    -- * Defining Drum Sets
    DrumID(..), drumBuffers, newDrum, getDrum,
    -- * Defining Tonal Insruments
    ToneInstrument, ToneID(..), ToneIndex(..), ToneTagSet, ToneTag(..), toneBuffers,
    newTone, getTone,
    -- * Mixing Tracks
    TrackSequence(..), sequenceToTrack,
  ) where

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Music.Composition
import           ProcGen.Music.KeyFreq88
import           ProcGen.Music.Synth

import           Control.Monad.State

import qualified Data.Map                    as Map
import qualified Data.Text                   as Strict
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as MUnboxed
import qualified Data.Vector.Mutable         as MBoxed
import           Data.Word

----------------------------------------------------------------------------------------------------

newtype Sequencer a = Sequencer (StateT Sequencer IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Buffers are all contained in a single mutable vector. This is their unique address.
newtype BufferID = BufferID Int
  deriving (Eq, Ord, Show)

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
data ToneID = ToneID !ToneIndex !ToneTagSet
  deriving (Eq, Ord, Show)

-- | Identify a sound by it's tone, or it's tone-transition (slide or cross-faded). This is not used
-- for drum kits.
data ToneIndex
  = KeyTone      !KeyIndex
  | SlideTone    !KeyIndex !KeyIndex
  | CrossFaded   !KeyIndex !KeyIndex
  deriving (Eq, Ord)

-- | Additional tags for a sound. A 'ToneID' has any number of these additional tags.
data ToneTag = Vibrato | Rolled | Muffled | Flubbed | Scratched
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype ToneTagSet = ToneTagSet (Unboxed.Vector Word8)
  deriving (Eq, Ord)

soundTagSet :: [ToneTag] -> ToneTagSet
soundTagSet = ToneTagSet . Unboxed.fromList . fmap (fromIntegral . fromEnum) . nub

instance Show ToneTagSet where
  show (ToneTagSet vec) = show $ toEnum . fromIntegral <$> Unboxed.toList vec

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

class BuffesSet set idx | set -> idx where
  buffers :: idx -> Lens' set (Maybe (Boxed.Vector BufferInfo))
  --getSound :: set -> idx -> Sequencer BufferInfo -- TODO

instance BufferSet ToneInstrument ToneID where { buffers = toneBuffers; }
instance BufferSet DrumInstrument DrumID where { buffers = drumBuffers; }

-- | Like 'buffers' but specific to the 'ToneID' and 'ToneInstrument' types.
toneBuffers :: ToneID -> Lens' ToneInstrument (Boxed.Vector BufferInfo)
toneBuffers i = lens (Map.lookup i . theToneTable) $ \ tone table ->
  tone{ theToneTable = Map.alter (const table) $ theToneTable tone }

drumBuffers :: DrumID -> Lens' DrumInstrument (Boxed.Vector BufferInfo)
drumBuffers i = lens (Map.lookup i . theDrumTable) $ \ drum table ->
  drum{ theDrumTable = Map.alter (const table) $ theDrumTable drum }

-- | Prepend 'BufferInfo' data to the front of the 'Boxed.Vector'.
addBuffer :: BufferInfo -> Boxed.Vector BufferInfo -> Boxed.Vector BufferInfo
addBuffer info = Boxed.fromList . (info :) . Boxed.toList

-- | Remove 'BufferInfo' data from some index of the 'Boxed.Vector'.
deleteBuffer :: Int -> Boxed.Vector BufferInfo -> Boxed.Vector BufferInfo
deleteBuffer i vec = if Boxed.length vec <= i || i < 0 then vec else Boxed.fromList $
  (vec Boxed.!) <$> ([0 .. i - 1] ++ [i + 1 .. Boxed.length vec - 1])

