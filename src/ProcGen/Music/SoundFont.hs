-- | Data types for "sound fonts," which are a collection sound effects indexed by tonal value. This
-- module also defines a 'DrumKit' type which is just an arbitrary, un-indexed collection of sounds.
-- ProcGen does not make use of any standardized or proprietary sound font formats, the sound fonts
-- defined in this module use a formatting unique to the ProcGen project.
module ProcGen.Music.SoundFont
  ( NoteID(..), NoteKeyIndicies(..),
    -- * Defining Drum Sets
    DrumID, DrumIndex(..), DrumValue, DrumKeyIndicies, DrumKit,
    drumValueIDs, drumValue, drumSounds, drumTable, addDrumToKit,
    -- * Defining Tonal Insruments
    InstrumentID(..), ToneID, ToneKeyIndicies, TiedTag(..), PlayNoteTag(..),
    ToneInstrument, addToneToInstrument, toneInstrument, toneSounds,
    PlayNoteTagSet, playNoteTagSet, toneTable, minMaxKeyIndex,
    -- * Working with Sound objects
    Sound, soundLikelyChoice, soundLoadedFromFile, soundRenderedFromFDSignal, soundTDSignal,
    soundFromFile, soundFromFDSignal, soundFromFDSignalFile,
    SoundSet, HasSoundSet(..), addSound, deleteSound, chooseSound,
  ) where

import           ProcGen.Types
import           ProcGen.Music.KeyFreq88
import           ProcGen.Music.Synth

import           Control.Lens
import           Control.Monad.Random

import           Data.List                  (nub, sort)
import qualified Data.Map                    as Map
import           Data.Semigroup
import           Data.String
import qualified Data.Text                   as Strict
import qualified Data.Vector                 as Boxed
import qualified Data.Vector.Unboxed         as Unboxed
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | A 'DrumIndex' is analogous to a 'ProcGen.Music.KeyFreq88.KeyIndex' in that it is used to select
-- a drum from a 'DrumKit'. The difference is, a 'DrumKit' is an unordered set of sound-making
-- elements, whereas an 88-key keyboard is an ordered set of sound-making elements (ordered in that
-- lower frequencies have lower index values).
newtype DrumIndex = DrumIndex { unwrapDrumIndex :: Strict.Text }
  deriving (Eq, Ord, Show)

instance IsString DrumIndex where
  fromString = DrumIndex . Strict.pack

----------------------------------------------------------------------------------------------------

newtype InstrumentID = InstrumentID Strict.Text
  deriving (Eq, Ord, Show)

instance IsString InstrumentID where { fromString = InstrumentID . Strict.pack; }

----------------------------------------------------------------------------------------------------

type ToneID = NoteID ToneValue
type DrumID = NoteID DrumValue

-- | Acts as a key that can be looked up in a map/dictionary, and serves as a unique descriptor of
-- the content of a buffer or a cluster of buffers. If there are more than one buffer associated
-- with a sound, when the sound is selected, one of the buffers is chosen at random.
--
-- This randomizing feature is designed to make an instrument sound more natural. For example, you
-- may have 3 versions of a violin sound playing the note A4 with
-- 'ProcGen.Music.Composition.MezzoForte' strength, perhaps one which is a slightly squeaky or
-- scratchy sound. Every time the sound A4 mezzo forte is selected, one of those three are seleted
-- from a 'Sequencer' using 'chooseSound'. The squeaky/scratchy sound can be weighted such that it
-- is randomly selected less often.
data NoteID value = NoteID !(NoteKeyIndicies value) !PlayNoteTagSet
  deriving (Eq, Ord, Show)

type ToneKeyIndicies = NoteKeyIndicies ToneValue
type DrumKeyIndicies = NoteKeyIndicies DrumValue

-- | Identify a sound by it's tone, or it's tone-transition (slide or cross-faded).
data NoteKeyIndicies value
  = NoteKey    !value
  | NoteTied   !TiedTag  !value !value
  deriving (Eq, Ord, Show)

-- | A tag used to indicate how notes are tied.
data TiedTag = TieNotes | CrossFadeNotes
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Additional tags for a sound. A 'ToneID' has any number of these additional tags.
data PlayNoteTag = Vibrato | Rolled | Muffled | Flubbed | Scratched
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype PlayNoteTagSet = PlayNoteTagSet (Unboxed.Vector Word8)
  deriving (Eq, Ord)

instance Show PlayNoteTagSet where
  show (PlayNoteTagSet vec) = show $ fromEnum . (fromIntegral :: Word8 -> Int) <$> Unboxed.toList vec

instance Semigroup PlayNoteTagSet where
  (PlayNoteTagSet a) <> (PlayNoteTagSet b) = PlayNoteTagSet $
    Unboxed.fromList $ nub $ Unboxed.toList a ++ Unboxed.toList b

instance Monoid PlayNoteTagSet where
  mempty = playNoteTagSet []
  mappend = (<>)

playNoteTagSet :: [PlayNoteTag] -> PlayNoteTagSet
playNoteTagSet = PlayNoteTagSet . Unboxed.fromList . fmap (fromIntegral . fromEnum) . nub

minMaxKeyIndex :: ToneKeyIndicies -> (KeyIndex, KeyIndex)
minMaxKeyIndex = \ case
  NoteKey    a   -> (minimum $ noteKeyIndicies a, maximum $ noteKeyIndicies a)
  NoteTied _ a b -> minmax2 a b
  where
    idx2    a b = noteKeyIndicies a ++ noteKeyIndicies b
    minmax2 a b = (minimum $ idx2 a b, maximum $ idx2 a b)

----------------------------------------------------------------------------------------------------

-- | A buffer contains meta-information about a 'TDSignal' constructed by a 'ProcGen.Music.Synth'.
data Sound
  = Sound
    { soundLoadedFromFile       :: !Strict.Text -- ^ might be null
    , soundRenderedFromFDSignal :: !(Maybe FDSignal)
    , soundTDSignal             :: !TDSignal
    , theSoundLikelyChoice      :: !Percentage
    }
  deriving Eq

instance Ord Sound where
  compare a b = theSoundLikelyChoice b `compare` theSoundLikelyChoice a

-- | When randomly choosing this sound from a list of sounds, how likely this sound will be
-- picked can be weighted by this value.
soundLikelyChoice :: Lens' Sound Percentage
soundLikelyChoice = lens theSoundLikelyChoice $ \ a b -> a{ theSoundLikelyChoice = b }

-- | Load a sound from a file without any specific meta-data about how the sound was constructed.
soundFromFile :: FilePath -> IO Sound
soundFromFile = error "TODO: ProcGen.Music.SoundFont.soundFromFile"

-- | Generate a sound from an 'FDSignal' using the functionality provided by the
-- "ProcGen.Music.Synth" module.
soundFromFDSignal :: FDSignal -> IO Sound
soundFromFDSignal = error "TODO: ProcGen.Music.SoundFont.soundFromFDSignal"

-- | Load a 'FDSignal' description from a file, then use 'soundFromFDSignal' to generate the
-- 'Sound'.
soundFromFDSignalFile :: FilePath -> IO Sound
soundFromFDSignalFile = error "TODO: ProcGen.Music.SoundFont.soundFromFDSignalFile"

----------------------------------------------------------------------------------------------------

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

-- | A 'DrumValue' is similar to a 'ToneValue' in that it contains one or more drums to play at a
-- given moment. Under the hood, the 'DrumValue' is simply a list of strings. Notcie that
-- 'DrumValue' instantiates 'Data.Semigroup.Semigroup' but not 'Data.Monoid.Monoid', because it is
-- meaningless to have an empty drum value in a sequencer: if a note exists it is played, if it is
-- not played it does not exist.
newtype DrumValue = DrumValue Strict.Text
  deriving (Eq, Ord)

instance Semigroup DrumValue where
  (DrumValue a) <> (DrumValue b) = DrumValue $
    Strict.intercalate (Strict.singleton '\0') $
    nub $ reverse $ breakByNulls b ++ breakByNulls a

data DrumKit = DrumKit { theDrumTable :: Map.Map DrumID SoundSet }

instance Semigroup DrumKit where
  (DrumKit a) <> (DrumKit b) = DrumKit $ Map.unionWith (<>) a b

instance Monoid DrumKit where { mempty = DrumKit mempty; mappend = (<>); }

drumTable :: Lens' DrumKit (Map.Map DrumID SoundSet)
drumTable = lens theDrumTable $ \ a b -> a{ theDrumTable = b }

addDrumToKit :: DrumID -> Sound -> DrumKit -> DrumKit
addDrumToKit key val = drumTable %~ Map.insertWith (<>) key (SoundSet $ Boxed.singleton val)

-- | Extract all 'DrumID' values that are stored in the 'DrumValue'.
drumValueIDs :: DrumValue -> [DrumIndex]
drumValueIDs (DrumValue str) = DrumIndex <$> breakByNulls str

-- not for export
breakByNulls :: Strict.Text -> [Strict.Text]
breakByNulls = filter (not . Strict.null) . flip loop [] where
  loop str stack = let (item, remainder) = Strict.breakOn (Strict.singleton '\0') str in
    (if Strict.null remainder then id else loop $ Strict.tail remainder) (item:stack)

drumValue :: DrumIndex -> [DrumIndex] -> DrumValue
drumValue a = DrumValue . Strict.intercalate (Strict.singleton '\0') .
  join . fmap (breakByNulls . unwrapDrumIndex) . (a :)

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
  let scale = sum $ theSoundLikelyChoice <$> Boxed.toList vec
  prob <- liftM (* scale) getRandom :: m Float
  -- This loop checks each element in the list in order, but it is expected that these vectors will
  -- be very small, usually around 4 elements or less.
  let loop n =  \ case
        []       -> return Nothing
        s:sounds -> seq n $! if n >= prob then return $ Just s else
          loop (n + theSoundLikelyChoice s) sounds
  loop 0 $ Boxed.toList vec
