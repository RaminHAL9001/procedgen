-- | A module for creating musical scores.
--
-- A composition consists of four different constraints to the random numbers that are procedurally
-- genrating the composition.
--
-- The first constraint is the "chord progression". A composition has an initial, repeating chord
-- progression, which might be interrupted by a "bridge" section of an alternative chord progression
-- that is played for a time before possibly returning to the initial chord progression.
--
-- The second constraint is that of a "Role", where each role represents a personality playing an
-- instrument. A personality is a representation of a person playing an instrument, however there is
-- not a one-to-one realtionship between roles and people. A single person may assume several roles
-- during a composition (like right and left hands on a piano), so what is encoded in this program
-- is roles, not people. Each role is defined as a beat, where each beat plays one or more notes,
-- where the notes played at any given time are constrained by the chord progression defined for
-- that point in time. There is an initial beat progression for each role which repeats, and then
-- there is an entropy value which randomizes the initial beat progression.
--
-- The third constraint is the "energy" of the composition, which has less to do with tempo and
-- volume, and more to do with the note density of a role at any given time. Each role is
-- randomizing their initial beat progression, the "energy" value determines how the beat
-- progression is randomized. A low energy value may see more notes elided and notes played held for
-- longer beats, and notes played not deviating from the initial beat quite so often. A high energy
-- value may see notes played as chords more often, and initial notes broken into multiple rapid
-- beats.
--
-- The fourth constraint is "interaction". This is a parameter that constrains exactly two roles to
-- interact with each other. An interaction is a single edige in a directed graph between two roles.
-- When two roles are "interacting" one role is leading, the other is following. This doesn't mean
-- the following role delays interaction, as musicians can predict what each other will do, and this
-- prediction results in simultaneous interaction. A following role tries to modify their beat to
-- complement the leading role's beat. This can be with counterpoint in which a follower's beats are
-- modified to play between lead's beats. It could also be with mimcry, in which a folower's beats
-- are modified to play simultaneously with the lead's beats. The number of notes that are modified
-- to follow is a tunable parameter.
module ProcGen.Music.Composition
  ( CommonChordProg(..),
    -- * Individual Notes
    Note(..), makeNote, Interval(..), NoteReference, untied,
    NoteValue(..), noteValue, Strength(..),
    -- * Arranging Notes
    Bar(..), makeBar,
    SubDiv(..), PlayedRole(..), playedRoleInstrument, playedRoleSequence,
    play1Note, sequenceBar, setNoteDurations,
    NoteSequence(..), listNoteSequence,
    -- * Composing Music for a Single Role
    Composition, evalComposition, randGenComposition,
    PlayableNote(..), note, rest, quick, tieNote, tie, untie,
    module ProcGen.Arbitrary,
    module Control.Monad.State.Class,
  ) where

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Music.KeyFreq88
import           ProcGen.Music.SoundFont

import           Control.Lens
import           Control.Monad.Primitive
import           Control.Monad.State
import           Control.Monad.State.Class
import           Control.Monad.ST

import qualified Data.IntMap               as IMap
import qualified Data.Map                  as Map
import           Data.Semigroup
import qualified Data.Vector.Mutable       as Mutable
import qualified Data.Vector.Unboxed       as Unboxed
import qualified Data.Vector               as Boxed
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | Common chord progressions
data CommonChordProg
  = AA_T24 -- A-A, 2/4 time signature
  | AB_T24 -- A-B, 2/4 time signature
  | AAA_T34 -- A-A-A, 3/4 time signature
  | AAB_T34 -- A-A-B, 3/4 time signature
  | ABC_T34 -- A-B-C, 3/4 time signature
  | AAAA_T44 -- A-A-A-A, 4/4 time signature
  | AAAB_T44 -- A-A-A-B chord progression, 4/4 time signature
  | ABAB_T44 -- A-B-A-B chord progression, 4/4 time signature
  | ABBA_T44 -- A-B-B-A chord progression, 4/4 time signature
  | ABCA_T44 -- A-B-C-A chord progression, 4/4 time signature
  | ABAC_T44 -- A-B-A-C chord progression, 4/4 time signature
  | ABCB_T44 -- A-B-C-B chord progression, 4/4 time signature
  | ABBC_T44 -- A-B-B-C chord progression, 4/4 time signature
  | ABCD_T44 -- A-B-C-D chord progression, 4/4 time signature
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

----------------------------------------------------------------------------------------------------

-- | A 'Note' with a 'ProcGen.Types.Duration'. Notes are associated with 'ProcGen.Types.Moment's
-- only in 'Map.Map' data structures within the 'Bar' data type.
data Interval note = Interval !Duration !note
  deriving (Eq, Ord, Show)

-- | A 'Composition' is constructed of a list of 'Note's each played at a point in time.
data Note
  = RestNote -- ^ indicates that nothing is played for the duration of the 'SubDiv'.
    -- ^ This must refer to a note that was played at an earlier point in the composition.
  | PlayedNote
    { playedTiedID    :: !NoteReference
      -- ^ Set to 'untied' if this note is not tied. If not 'untied' (if it is tied) it must refer
      -- to the 'playedNotID' of 'Note' defined with the 'TiedNote' constructor.
    , playedStrength  :: !Strength
    , playedNoteValue :: !NoteValue
    }
  deriving (Eq, Ord, Show)

-- | Construct a note from a 'Strength' and zero or more 'ProcGen.Music.KeyFreq88.KeyIndex' values
-- which refer to notes on an 88-key piano keyboard.
makeNote :: NoteReference -> Strength -> [KeyIndex] -> Note
makeNote tied strength = \ case
  []   -> RestNote
  a:ax -> PlayedNote
    { playedTiedID    = untied
    , playedStrength  = strength
    , playedNoteValue = noteValue a ax
    }

-- | Every note in a 'Composition' has a unique index value or "reference". The 'Composition' keeps
-- track of each unique note played in order to evaluate "tied notes" which are notes are held for
-- an amount of time, sometimes sliding (gradually changing the sound'S base frequency) into another
-- note. 
newtype NoteReference = NoteReference Int deriving (Eq, Ord, Show)

-- | A 'Note' that is not tied.
untied :: NoteReference
untied = NoteReference minBound

-- | The value of the note played in a 'Note'.
data NoteValue
  = Single !Word8
    -- ^ Play a single note, the 'Data.Wordl.Word8' value will be used as an index to a table
    -- constructed by a 'keySigFreqTable' for whatever key signature a given bar is played in.
  | Chord  !(Unboxed.Vector Word8) -- ^ Like a 'Single' but playes several notes simultaneously.
  deriving (Eq, Ord, Show, Read)

-- | How hard the note is played, it is a fuzzy value that maps to 'ProcGen.Types.Amplitude'.
data Strength = Pianismo | Piano | MezzoPiano | MezzoMezzo | MezzoForte | Forte | Fortisimo
  deriving (Eq, Ord, Show, Read, Enum)

-- | Construct a 'NoteValue'. It is better to just use 'makeNote' rather than ever call this
-- function directly.
noteValue :: KeyIndex -> [KeyIndex] -> NoteValue
noteValue a = \ case
  [] -> Single $ keyIndexToWord8 a
  ax -> Chord $ Unboxed.fromList $ keyIndexToWord8 <$> a:ax

----------------------------------------------------------------------------------------------------

-- | A 'Bar' is a single unit of a composition that can be constructed by the stateful function type
-- 'ComposeRoleT'. 'ComposeRoleT' will subdivide a 'Bar' into units and then play a sequence of
-- notes at each unit. The constructed 'Bar' can then be translated into a 'NoteSequence' using the
-- 'sequenceBar' function.
data Bar leaf
  = Bar
    { playMetaOffset :: !Percentage
      -- ^ Wait an amount of time, specified as a percentage of the measure play time, before
      -- playing the notes defined in this measure.
    , playMetaCutoff :: !Percentage
      -- ^ Stop playing after some percentage of the measure play time after the 'playMetaOffset'
      -- has begun, regardless of whether unplayed notes remain in this measure. For example, if the
      -- play offset is 0.25 and you want to play only to the end of the measure and no more beyond
      -- it, specify 0.75 for 'playMetaCutoff'. Basically, the 'playMetaOffset' and 'playMetaCutoff'
      -- add together to specify the point in time after the start of the measure where the measure
      -- starts and ends. If the sum of these two values is greater than 1.0, the notes will be
      -- played beyond the end of the measure.
    , playNoteTree   :: !(SubDiv leaf)
    }
  deriving Functor

makeBar :: Bar leaf
makeBar = Bar
  { playMetaOffset = 0.0
  , playMetaCutoff = 1.0
  , playNoteTree   = SubDivBranch Boxed.empty
  }

----------------------------------------------------------------------------------------------------

-- | A mapping from 'ProcGen.Types.Moment's in time to @note@s. Essentially this is just a map data
-- structure that instantiates 'Data.Semigroup.Semigroup' and 'Data.Monoid.Monoid' such that the
-- append @('Data.Semigroup.<>')@ function performs the right-biased union of the maps, where
-- right-bias meaning the @note@ on the right of the @('Data.Semigroup.<>')@ operator overwrites the
-- @note on the left of the operator if the two operators appear in the exact same
-- 'ProcGen.Types.Moment' in time.
newtype NoteSequence note = NoteSequence (Map.Map Moment note)
  deriving (Eq, Functor)

-- | A data structure for sub-dividing a measure of time. A 'SubDiv' contains more structure than a
-- 'NoteSequence' but ultimately must be translated to a 'NoteSequence' to be of any use.
data SubDiv leaf
  = SubDivLeaf   !leaf
  | SubDivBranch !(Boxed.Vector (SubDiv leaf))
  deriving Functor

instance Semigroup (NoteSequence note) where
  (NoteSequence a) <> (NoteSequence b) = NoteSequence (Map.union b a)

instance Monoid (NoteSequence note) where
  mempty = NoteSequence mempty
  mappend = (<>)

listNoteSequence :: NoteSequence note -> [(Moment, note)]
listNoteSequence (NoteSequence map) = Map.assocs map

----------------------------------------------------------------------------------------------------

play1Note :: Moment -> Duration -> note -> NoteSequence (Duration, note)
play1Note t dt = NoteSequence . Map.singleton t . (,) dt

-- | A 'Bar' sub-divides the given initial 'ProcGen.Types.Duration' into several sub-intervals
-- associated with the leaf elements. This function converts a 'Measure' into a mapping from the
-- start time to the @('ProcGen.Types.Duration', leaf)@ pair. When the @leaf@ type is unified with
-- 'Note', it is helpful to evaluate the resulting 'PlayedRole' with 'setNoteDurations'.
sequenceBar :: Moment -> Duration -> Bar note -> NoteSequence (Duration, note)
sequenceBar t0 dt0 msur = loop dt0 mempty (t0 + playMetaOffset msur, playNoteTree msur) where
  loop dt0 map (t0, subdiv) = if t0 >= playMetaCutoff msur then map else case subdiv of
    SubDivLeaf  note -> map <> play1Note t0 dt0 note
    SubDivBranch vec -> if Boxed.null vec then mempty else
      let dt = dt0 / realToFrac (Boxed.length vec) in
      foldl (loop dt) map $ zip (iterate (+ dt) t0) (Boxed.toList vec)
 -- TODO: ^ make this function interpret tied notes.

-- | For a 'PlayedRole' containing a tuple @('Duration', 'Note')@ pair as what is constructed
-- by the 'sequenceBar' function, this sets the 'ProcGen.Types.Duration' value in the
-- 'Prelude.fst' of the tuple as the 'playedDuration' of each 'Note' in the 'Prelude.snd' of
-- the tuple.
setNoteDurations :: NoteSequence (Duration, Note) -> NoteSequence (Interval Note)
setNoteDurations = fmap $ uncurry Interval

----------------------------------------------------------------------------------------------------

-- | This is a 'NoteSequence' associated with a 'ProcGen.Music.SoundFont.InstrumentID'.
data PlayedRole note
  = PlayedRole
    { thePlayedRoleInstrument :: !InstrumentID
    , thePlayedRoleSequence   :: !(NoteSequence note)
    }
  deriving (Eq, Functor)

playedRoleInstrument :: Lens' (PlayedRole note) InstrumentID
playedRoleInstrument = lens thePlayedRoleInstrument $ \ a b -> a{ thePlayedRoleInstrument = b }

playedRoleSequence :: Lens' (PlayedRole note) (NoteSequence note)
playedRoleSequence = lens thePlayedRoleSequence $ \ a b -> a{ thePlayedRoleSequence = b }

----------------------------------------------------------------------------------------------------

-- | This is a monadic function type designed so you can construct the 'Bar's of an individual role
-- for a piece of music using @do@ notation. Functions of this data type such as 'note', 'rest',
-- 'quick', 'tie', and 'untie'.
--
-- One aspect that may be confusing about the 'Composition' function type is that the state of the
-- 'Composition' does not keep track of time at all. Instead, a tree structure of notes of type
-- 'SubDiv' is constructed. It is only after evaluating the 'Composition' using 'runComposition'
-- that you can extract the 'SubDiv' and convert it to a timed sequence of notes using
-- @('setNoteDurations' . 'sequenceBar')@.
newtype Composition a = Composition (StateT CompositionState (TFRandT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

data CompositionState
  = CompositionState
    { theComposeTieID        :: !Int
    , theComposeTiedNotes    :: !(IMap.IntMap Note)
    , theComposeKeySignature :: [ToneIndex]
    , theComposeNotes        :: [SubDiv Note]
    }

instance MonadStatae CompositionState Composition where
  state = Composition . lift . state

instance Semigroup a => Monoid (Composition a) where { a <> b = (<>) <$> a <*> b; }

instance Monoid a => Monoid (Composition a) where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b

instance MonadRandom Composition where
  getRandomR  = Composition . lift . getRandomR
  getRandom   = Composition $ lift getRandom
  getRandomRs = Composition . lift . getRandomRs
  getRandoms  = Composition $ lift getRandoms

composeTieID :: Lens' CompositionState Int
composeTieID = lens theComposeTieID $ \ a b -> a{ theComposeTieID = b }

composeTiedNotes :: Lens' CompositionState (IMap.IntMap Note)
composeTiedNotes = lens theComposeTiedNotes $ \ a b -> a{ theComposeTiedNotes = b }

composeKeySignature :: Lens' CompositionState [ToneIndex]
composeKeySignature = lens theComposeKeySignature $ \ a b -> a{ theComposeKeySignature = b }

composeNotes :: Lens' CompositionState [SubDiv Note]
composeNotes = lens theComposeNotes $ \ a b -> a{ theComposeNotes = b }

class PlayableNote n where
  -- | Convert some value to a 'Note' so it can be played by 'note'. The unit @()@ data type
  -- instantiates this class such that @()@ can be used to indicate a 'RestNote'.
  toNote :: n -> Composition Note

instance PlayableNote ()   where { toNote = return RestNote; }
instance PlayableNote Note where { toNote = return; }

-- | Play a note
note :: PlayableNote n => n -> Composition ()
note = toNote >=> (%=) composeNotes . (:) . SubDivLeaf

-- | Leave a brief silent gap in the current 'SubDiv' (a gap in the sub-division of the current
-- interval).
rest :: Composition ()
rest = note RestNote

-- | Sieze the current time 'Interval' of the current 'SubDiv', and begin sub-dividing it with every
-- note played.
quick :: Composition () -> Composition ()
quick subcomposition = do
  oldnotes  <- use composeNotes <* (composeNotes .= [])
  subcomposition
  notes <- use composeNotes
  composeNotes .= SubDivBranch (Boxed.fromList notes) : oldnotes

-- | Play a 'note' that will be tied to another 'note' at some point in the future. The tied note is
-- held for a time until the future 'untie'd note is reached.
tieNote :: PlayableNote n => n -> Composition (NoteReference, Note)
tieNote = toNote >=> \ case
  n@PlayedNote{} -> do
    i <- composeTieID += 1 >> use composeTieID
    composeNotes %= (:) n{ playedTiedID = NoteReference i }
    return (NoteReference i, n)
  n -> do
    return (untied, n)

-- | Shorthand for 'tieNote' returning only the 'NoteReference' and not the 'Note' constructed with
-- it.
tie :: PlayeableNote n => n -> Composition NoteReference
tie = fmap fst . tieNote

-- | Stop playing a tied 'note'. Supply the 'NoteReference' returend by a previous call to 'tie',
-- pass the note value to which the note must be tied as @note1@ (passing 'RestNote' means to tie
-- the same note that initiated the 'tie'). The value @note2@ can be played with the note that is
-- being united at the time interval it is untied.
untie
  :: (PlayableNote note1, PlayableNote note2)
  => NoteReference -> note1 -> note2 -> Composition ()
untie (NoteReference ref) n1 n2 = do
  n1 <- note n1
  n1 <- case n1 of
    RestNote -> maybe RestNote id . IMap.lookup ref <$> use composeTiedNotes
    n1       -> pure n1
  case n1 of
    RestNote -> return ()
    _        -> note n1
  note n2
