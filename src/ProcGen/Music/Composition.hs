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
    ScoredNote(..), scoreNote,
    PlayedNote(..), playScoredNote,
    NoteReference, untied,
    Strength(..),
    -- * Arranging Notes
    Bar(..), makeBar,
    SubDiv(..), PlayedRole(..), playedRoleInstrument, playedRoleSequence,
    NoteSequence(..), listNoteSequence, playNoteSequence,
    -- * Composing Music for a Single Role
    Composition, evalComposition, randGenComposition,
    ScorableNote(..), note, rest, quick, tieNote, tie, untie,
    module ProcGen.Arbitrary,
    module Control.Monad.State.Class,
  ) where

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Music.KeyFreq88
import           ProcGen.Music.SoundFont

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.State.Class

import qualified Data.IntMap               as IMap
import qualified Data.Map                  as Map
import           Data.Semigroup
import qualified Data.Vector.Mutable       as Mutable
import qualified Data.Vector               as Boxed

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

-- | A 'Composition' is constructed of a list of 'ScoredNote's each played at a point in time. A
-- 'ScoredNote' is different from a 'PlayedNote' in that a 'ScoredNote' is an element of a
-- 'Composition' whereas a 'PlayedNote' is an instruction to a synthesizer to play a particular
-- note.
data ScoredNote
  = ScoredRestNote -- ^ indicates that nothing is played for the duration of the 'SubDiv'.
    -- ^ This must refer to a note that was played at an earlier point in the composition.
  | ScoredNote
    { scoredTiedID    :: !NoteReference
      -- ^ Set to 'untied' if this note is not tied. If not 'untied' (if it is tied) it must refer
      -- to the 'playedNotID' of 'Note' defined with the 'TiedNote' constructor.
    , scoredStrength  :: !Strength
    , scoredNoteValue :: !NoteValue
    , scoredNoteTags  :: !ToneTagSet
    }
  deriving (Eq, Ord, Show)

-- | A 'ScoredNote' that has been sequenced using 'sequenceBar'. This data type is basically an
-- instruction to a synthesizer on what note to play.
data PlayedNote
  = RestNote
  | PlayedNote
    { playedDuration  :: !Duration
    , playedStrength  :: !Strength
    , playedNoteValue :: !ToneID
    , playedTied      :: !PlayedNote
    }

-- | How hard the note is played, it is a fuzzy value that maps to 'ProcGen.Types.Amplitude'.
data Strength = Pianismo | Piano | MezzoPiano | MezzoMezzo | MezzoForte | Forte | Fortisimo
  deriving (Eq, Ord, Show, Read, Enum)

-- | Construct a note from a 'Strength' and zero or more 'ProcGen.Music.KeyFreq88.KeyIndex' values
-- which refer to notes on an 88-key piano keyboard.
scoreNote :: NoteReference -> [ToneTag] -> Strength -> [KeyIndex] -> ScoredNote
scoreNote tied tags strength = \ case
  []   -> ScoredRestNote
  a:ax -> ScoredNote
    { scoredTiedID    = tied
    , scoredStrength  = strength
    , scoredNoteValue = noteValue a ax
    , scoredNoteTags  = toneTagSet tags
    }

-- | Convert a 'ScoredNote' to a 'PlayedNote'.
playScoredNote :: Duration -> ScoredNote -> PlayedNote
playScoredNote dt = \ case
  ScoredRestNote -> RestNote
  note           -> PlayedNote
    { playedDuration  = dt
    , playedStrength  = scoredStrength note
    , playedNoteValue = ToneID (KeyTone $ scoredNoteValue note) (scoredNoteTags note)
    , playedTied      = RestNote
    }

----------------------------------------------------------------------------------------------------

-- | Every note in a 'Composition' has a unique index value or "reference". The 'Composition' keeps
-- track of each unique note played in order to evaluate "tied notes" which are notes are held for
-- an amount of time, sometimes sliding (gradually changing the sound'S base frequency) into another
-- note. 
newtype NoteReference = NoteReference Int deriving (Eq, Ord, Show)

-- | A 'Note' that is not tied.
untied :: NoteReference
untied = NoteReference minBound

----------------------------------------------------------------------------------------------------

-- | A 'Bar' is a single unit of a composition that can be constructed by the stateful function type
-- 'ComposeRoleT'. 'ComposeRoleT' will subdivide a 'Bar' into units and then play a sequence of
-- notes at each unit. The constructed 'Bar' can then be translated into a 'NoteSequence' using the
-- 'playNoteSequence' function.
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

-- | Construct a lazy list in reverse order (so you can 'Prelude.foldr' over it in forward
-- order). The result of this function evaluation is intended to be passed to other intermediate
-- computations before producing a final 'NoteSequence' data structure.
sequenceBar :: Moment -> Duration -> Bar note -> [(Moment, Duration, note)]
sequenceBar t0 dt0 msur = loop dt0 (t0 + playMetaOffset msur, playNoteTree msur) [] where
  loop dt0 (t0, subdiv) list = if t0 >= playMetaCutoff msur then list else case subdiv of
    SubDivLeaf  note -> (t0, dt0, note) : list
    SubDivBranch vec -> if Boxed.null vec then mempty else
      let dt = dt0 / realToFrac (Boxed.length vec) in
      foldr (loop dt) list $ zip (iterate (+ dt) t0) (Boxed.toList vec)

-- | A 'Prelude.foldr' over the result of 'sequenceBar' in which 'PlayedNotes' that share the same
-- 'NoteReference' in their 'scoredTiedID' are "tied" together, meaning all tied notes are linked
-- together into a single 'PlayedNote' structure where each next tied note is set in the
-- 'playedTied' field of the 'PlayedNote' data structure. Keep in mind that the tied notes are
-- appended to the result list so this function causes notes to be listed out of order. Use
-- 'playNoteSequence' to re-order the elements in this list.
tieSequencedNotes :: [(Moment, Duration, ScoredNote)] -> [(Moment, PlayedNote)]
tieSequencedNotes = uncurry (flip (++)) . fmap makeTied . foldr f ([], IMap.empty) where
  f (t, dt, scor) (list, table) = if untied == scoredTiedID scor then (list, table) else
    let (NoteReference i) = scoredTiedID scor
        play = (t, playScoredNote dt scor)
    in (list, IMap.alter (Just . maybe [play] (play :)) i table)
  tie (t2, n2) (t1, n1) = case n1 of
    RestNote     -> (t2, n2)
    PlayedNote{} -> (t1, n1{ playedTied = n2, playedDuration = playedDuration n2 + t2 - t1})
  makeTied = fmap (foldl1 tie) . IMap.elems

----------------------------------------------------------------------------------------------------

-- | A mapping from 'ProcGen.Types.Moment's in time to @note@s. Essentially this is just a map data
-- structure that instantiates 'Data.Semigroup.Semigroup' and 'Data.Monoid.Monoid' such that the
-- append @('Data.Semigroup.<>')@ function performs the right-biased union of the maps, where
-- right-bias meaning the @note@ on the right of the @('Data.Semigroup.<>')@ operator overwrites the
-- @note on the left of the operator if the two operators appear in the exact same
-- 'ProcGen.Types.Moment' in time.
newtype NoteSequence note = NoteSequence (Map.Map Moment [note])
  deriving (Eq, Functor)

-- | A data structure for sub-dividing a measure of time. A 'SubDiv' contains more structure than a
-- 'NoteSequence' but ultimately must be translated to a 'NoteSequence' to be of any use.
data SubDiv leaf
  = SubDivLeaf   !leaf
  | SubDivBranch !(Boxed.Vector (SubDiv leaf))
  deriving (Eq, Functor)

instance Semigroup (NoteSequence note) where
  (NoteSequence a) <> (NoteSequence b) = NoteSequence (Map.union b a)

instance Monoid (NoteSequence note) where
  mempty = NoteSequence mempty
  mappend = (<>)

listNoteSequence :: NoteSequence note -> [(Moment, [note])]
listNoteSequence (NoteSequence map) = Map.assocs map

-- | A 'Bar' sub-divides the given initial 'ProcGen.Types.Duration' into several sub-intervals
-- associated with the leaf elements. This function converts a 'Measure' into a mapping from the
-- start time to the @('ProcGen.Types.Duration', leaf)@ pair. When the @leaf@ type is unified with
-- 'Note', it is helpful to evaluate the resulting 'PlayedRole' with 'setNoteDurations'.
playNoteSequence :: Moment -> Duration -> Bar ScoredNote -> NoteSequence PlayedNote
playNoteSequence t0 dt0 = NoteSequence
  . foldr (uncurry $ Map.insertWith (++)) Map.empty
  . fmap (fmap pure) . tieSequencedNotes . sequenceBar t0 dt0

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
    , theComposeNoteCount    :: !Int
    , theComposeKeySignature :: [ToneIndex]
    , theComposeNotes        :: [SubDiv ScoredNote]
    }

instance MonadState CompositionState Composition where
  state = Composition . state

instance Semigroup a => Semigroup (Composition a) where { a <> b = (<>) <$> a <*> b; }

instance Monoid a => Monoid (Composition a) where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b

instance MonadRandom Composition where
  getRandomR  = Composition . lift . getRandomR
  getRandom   = Composition $ lift getRandom
  getRandomRs = Composition . lift . getRandomRs
  getRandoms  = Composition $ lift getRandoms

emptyCompositionState :: CompositionState
emptyCompositionState = CompositionState
  { theComposeTieID        = 0
  , theComposeNoteCount    = 0
  , theComposeKeySignature = []
  , theComposeNotes        = []
  }

composeTieID :: Lens' CompositionState Int
composeTieID = lens theComposeTieID $ \ a b -> a{ theComposeTieID = b }

composeKeySignature :: Lens' CompositionState [ToneIndex]
composeKeySignature = lens theComposeKeySignature $ \ a b -> a{ theComposeKeySignature = b }

composeNotes :: Lens' CompositionState [SubDiv ScoredNote]
composeNotes = lens theComposeNotes $ \ a b -> a{ theComposeNotes = b }

composeNoteCount :: Lens' CompositionState Int
composeNoteCount = lens theComposeNoteCount $ \ a b -> a{ theComposeNoteCount = b }

----------------------------------------------------------------------------------------------------

class ScorableNote n where
  -- | Convert some value to a 'Note' so it can be played by 'note'. The unit @()@ data type
  -- instantiates this class such that @()@ can be used to indicate a 'RestNote'.
  toNote :: n -> Composition ScoredNote

instance ScorableNote ()         where { toNote = const $ return ScoredRestNote; }
instance ScorableNote ScoredNote where { toNote = return; }

-- | Evaluate a 'Composition' function automatically seeding a new random number generator using
-- 'Control.Random.TF.Init.initTFGen'.
evalComposition :: Composition a -> IO a
evalComposition (Composition f) = liftM fst $
  seedIORunTFRandT $ evalStateT f emptyCompositionState 

randGenComposition :: Composition a -> TFGen -> IO (a, TFGen)
randGenComposition (Composition f) = runTFRandT (evalStateT f emptyCompositionState)

-- | Play a note
note :: ScorableNote n => n -> Composition ()
note n = toNote n >>= (%=) composeNotes . (:) . SubDivLeaf >> composeNoteCount += 1

-- | Leave a brief silent gap in the current 'SubDiv' (a gap in the sub-division of the current
-- interval).
rest :: Composition ()
rest = note ScoredRestNote

-- | Sieze the current time 'Interval' of the current 'SubDiv', and begin sub-dividing it with every
-- note played.
quick :: Composition () -> Composition ()
quick subcomposition = do
  oldnotes <- use composeNotes     <* (composeNotes     .= [])
  oldcount <- use composeNoteCount <* (composeNoteCount .= 0)
  subcomposition
  notes    <- use composeNotes
  count    <- use composeNoteCount
  let mkvec count elems = Boxed.create $ do
        vec <- Mutable.new count
        mapM_ (uncurry $ Mutable.write vec) $ zip (iterate (subtract 1) (count - 1)) elems
        return vec
  composeNotes     .= SubDivBranch (mkvec count notes) : oldnotes
  composeNoteCount .= oldcount + 1

-- | Play a 'note' that will be tied to another 'note' at some point in the future. The tied note is
-- held for a time until the future 'untie'd note is reached.
tieNote :: ScorableNote n => n -> Composition (NoteReference, ScoredNote)
tieNote = toNote >=> \ case
  n@ScoredNote{} -> do
    i <- composeTieID += 1 >> use composeTieID
    composeNotes %= (:) (SubDivLeaf n{ scoredTiedID = NoteReference i })
    return (NoteReference i, n)
  n -> do
    return (untied, n)

-- | Shorthand for 'tieNote' returning only the 'NoteReference' and not the 'Note' constructed with
-- it.
tie :: ScorableNote n => n -> Composition NoteReference
tie = fmap fst . tieNote

-- | Stop playing a tied 'note'. Supply the 'NoteReference' returend by a previous call to 'tie',
-- pass the note value to which the note must be tied as @note1@ (passing 'RestNote' means to tie
-- the same note that initiated the 'tie'). The value @note2@ can be played with the note that is
-- being united at the time interval it is untied.
untie :: ScorableNote note => NoteReference -> note -> Composition ()
untie ref = toNote >=> \ case
  ScoredRestNote -> return ()
  n -> note n{ scoredTiedID = ref }
