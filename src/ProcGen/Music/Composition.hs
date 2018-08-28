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
import           ProcGen.Music.KeyFreq88

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
data SubMeasure a
  = M1 !a
  | M2 !a !a
  | M3 !a !a !a
  | M4 !a !a !a !a
  deriving (Eq, Ord, Show, Read)

subMeasureToList :: SubMeasure a -> (Duration, [(Moment, a)])
subMeasureToList = fmap (zip [0.0, 1.0 ..]) . \ case
  M1 a         -> (1, [a])
  M2 a b       -> (2, [a, b])
  M3 a b c     -> (3, [a, b, c])
  M4 a b c d   -> (4, [a, b, c, d])

flattenSubMeasure
  :: (Moment -> Duration -> a -> [(Moment, Duration, b)])
  -> Moment -> Duration -> SubMeasure a -> [(Moment, Duration, b)]
flattenSubMeasure subdiv t0 dt0 measure = do
  let (div, elems) = subMeasureToList measure
  let dt = dt0 / div
  (offset, elem) <- elems
  subdiv (t0 + offset*dt) dt elem

----------------------------------------------------------------------------------------------------

newtype Measure = Measure (SubMeasure (SubMeasure PlayedNote))
  deriving (Eq, Ord, Show, Read)

subMeasures :: Measure -> SubMeasure (SubMeasure PlayedNote)
subMeasures (Measure a) = a

----------------------------------------------------------------------------------------------------

-- | A beat pattern is defined as a way to sub-divide a measure of time. The number of elements @N@
-- in the 'BeatPattern' defines the time signature of the song as @N/4@, so @N@ must be 2 or more,
-- and should probably be less than 10 for most songs.
newtype BeatPattern = BeatPattern (Boxed.Vector Measure)
  deriving (Eq, Ord, Show, Read)

-- | The number of time divisions in a bar, where a bar consists of @N@ 'Measure's where @N@ is the
-- top value of the time signature.
beatTimeDiv :: BeatPattern -> Duration
beatTimeDiv (BeatPattern vec) = realToFrac $ Boxed.length vec

beatToMeasures :: BeatPattern -> [Measure]
beatToMeasures (BeatPattern vec) = Boxed.toList vec

beatToSubMeasures :: BeatPattern -> [SubMeasure (SubMeasure PlayedNote)]
beatToSubMeasures = fmap subMeasures . beatToMeasures

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
-- 'BeatPattern' into a simple temporal list of 'PlayedNotes'.
roleNotes :: BeatPattern -> RoleNotes
roleNotes beat = RoleNotes $ Map.fromList $ do
  (t, measure) <- zip [0.0, 1.0 ..] (beatToSubMeasures beat)
  (t, dt, note) <- flattenSubMeasure (flattenSubMeasure $ \ t dt note -> (t, dt, note)) t 1.0
  [(t, note{ playedDuration = dt })]

----------------------------------------------------------------------------------------------------


