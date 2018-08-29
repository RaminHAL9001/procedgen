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
module ProcGen.Music.Composition where

import           ProcGen.Types
--import           ProcGen.Music.KeyFreq88

import qualified Data.Map                  as Map
import           Data.Semigroup
import qualified Data.Vector.Unboxed       as Unboxed
import qualified Data.Vector               as Boxed
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | Common chord progressions
data CommondChordProg
  = S1_T24 -- Single chord progression, 2/4 time signature
  | S2_T24 -- Dual chord progression, 2/4 time signature
  | S1_T34 -- Single chord progression, 3/4 time signature
  | S3_T34 -- Tripple chord progression, 3/4 time signature
  | S1_T44 -- Single chord progression, 4/4 time signature
  | ABAB_T44 -- A-B-A-B chord progression, 4/4 time signature
  | ABBA_T44 -- A-B-B-A chord progression, 4/4 time signature
  | ABCA_T44 -- A-B-C-A chord progression, 4/4 time signature
  | ABAC_T44 -- A-B-A-C chord progression, 4/4 time signature
  | S4_T44 -- Quad chord progression, 4/4 time signature
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

----------------------------------------------------------------------------------------------------

data PlayedNote
  = PlayedNote
    { playedDuration  :: !Duration
    , playedStrength  :: !Strength
    , playedTied      :: !TiedNote
    , playedNoteValue :: !NoteValue
    }
  deriving (Eq, Ord, Show, Read)

-- | The value of the note played in a 'PlayedNote'.
data NoteValue
  = Rest
    -- ^ If the note is preceeded by another tied note, sustain the note through this rest and the
    -- next rest intervals.
  | Single !Word8
    -- ^ Play a single note. If the note is tied and followed by a 'Rest', the note is held through
    -- a single rest interval. If the note is tied and followed by another note, the note is held
    -- until the next note is played.
  | Chord  !(Unboxed.Vector Word8) -- ^ Play a chord
  | Trill  !(Unboxed.Vector Word8) -- ^ Play a chord trilled
  deriving (Eq, Ord, Show, Read)

-- | If a 'PlayedNote' is followed by a 'Rest' and the note is 'Tied', the note is held for the
-- duration of the rest. 'Legato' is the deafult state, the fade-out of a note played overlaps with
-- the next note played. 'Staccato' shortens note so the fade out completes before the next note is
-- played.
data TiedNote = Legato | Staccato | Tied
  deriving (Eq, Ord, Show, Read, Enum)

-- | How hard the note is played.
data Strength = Fortisimo | Forte | MezzoForte | MezzoMezzo | MezzoPiano | Piano | Pianismo
  deriving (Eq, Ord, Show, Read, Enum)

----------------------------------------------------------------------------------------------------

-- | A data structure for sub-dividing a measure of time.
data SubDiv leaf
  = SubDivLeaf   !leaf
  | SubDivBranch !(Boxed.Vector (SubDiv leaf))
  deriving (Eq, Ord, Show, Read)

----------------------------------------------------------------------------------------------------

data Measure leaf
  = Measure
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
  deriving (Eq, Ord, Show, Read)

-- | A 'Measure' sub-divides the given initial 'ProcGen.Types.Duration' into several sub-intervals
-- associated with the leaf elements. This function converts a 'Measure' into a mapping from the
-- start time to the @('ProcGen.Types.Duration', leaf)@ pair.
sequenceMeasure :: Moment -> Duration -> Measure a -> Map.Map Moment (Duration, a)
sequenceMeasure t0 dt0 msur = loop Map.empty (t0 + playMetaOffset msur) dt0 (playNoteTree msur) where
  loop :: Map.Map Moment (Duration, a) -> Moment -> Duration -> SubDiv a -> Map.Map Moment (Duration, a)
  loop map t0 dt0 subdiv = if t0 >= playMetaCutoff msur then map else case subdiv of
    SubDivLeaf  leaf -> Map.union map $ Map.singleton t0 (dt0, leaf)
    SubDivBranch vec -> if Boxed.null vec then Map.empty else
      let dt = dt0 / realToFrac (Boxed.length vec) in
        foldl (\ map (t, a) -> loop map t dt a) map $ zip (iterate (+ dt) t0) (Boxed.toList vec)

----------------------------------------------------------------------------------------------------

-- | The notes played by a single 'Role' at each point in time. The time the note played is defined
-- by the index of the 'Map.Map', where each measure contains up to @measureTimeDiv@ elements.
newtype RoleNotes = RoleNotes (Map.Map Moment PlayedNote)
  deriving Eq

instance Semigroup RoleNotes where
  (RoleNotes a) <> (RoleNotes b) = RoleNotes (Map.union b a)

instance Monoid RoleNotes where
  mempty = RoleNotes mempty
  mappend = (<>)

-- | Construct a 'RoleNotes' from a 'Beat'. Music is composed of 'BeatPattern's, but the actual
-- notes played, the actual execution of the musical score, cannot be done without collapsing a
-- 'BeatPattern' into a simple temporal list of 'PlayedNotes'. The input for this function comes
-- from 'sequenceMeasure' where the type sequenced is of @('Measure' 'PlayedNote')@.
roleNotes :: Map.Map Moment (Duration, PlayedNote) -> RoleNotes
roleNotes = RoleNotes . fmap (\ (t, note) -> note{ playedDuration = t })
