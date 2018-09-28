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
    PlayedNote(..), playScoredNote, getTiedNotes,
    NoteReference, untied,
    Strength(..),  strengthToAmplitude,
    -- * Arranging Notes
    Bar, PlayedRole(..), playedRoleInstrument, playedRoleSequence, nextBar,
    NoteSequence(..), listNoteSequence, playNoteSequence,
    -- * Composing Music for a Single Role
    Composition, evalComposition, randGenComposition, composeKeySignature,
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
  = ScoredRestNote -- ^ indicates that nothing is played for the duration of the 'Bar'.
    -- ^ This must refer to a note that was played at an earlier point in the composition.
  | ScoredNote
    { scoredTiedID    :: !NoteReference
      -- ^ Set to 'untied' if this note is not tied. If not 'untied' (if it is tied) it must refer
      -- to the 'playedNoteID' of 'Note' defined with the 'tieNote' constructor.
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
data Strength = Pianismo | Piano | MezzoPiano | Moderare | MezzoForte | Forte | Fortisimo
  deriving (Eq, Ord, Show, Read, Enum)

strengthToAmplitude :: Strength -> Amplitude
strengthToAmplitude = \ case
  Pianismo   -> 1/7
  Piano      -> 2/7
  MezzoPiano -> 3/7
  Moderare   -> 4/7
  MezzoForte -> 5/7
  Forte      -> 6/7
  Fortisimo  -> 7/7

-- | Extract a sequence of all tied notes from a 'PlayedNote'. This will re-write the 'ToneID's of
-- the 'playedNoteValue's. If any of the 'PlayedNote's are already slide notes, their existing
-- slides will be over-written to ensure the list of 'PlayedNote's produced slide from one to the
-- next smoothly.
getTiedNotes :: PlayedNote -> [PlayedNote]
getTiedNotes = \ case
  RestNote -> []
  note1    -> case playedTied note1 of
    RestNote -> [note1]
    note2    -> note1 : getTiedNotes note2

-- | Construct a note from a 'Strength' and zero or more 'ProcGen.Music.KeyFreq88.KeyIndex' values
-- which refer to notes on an 88-key piano keyboar.d
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

-- | A data structure for sub-dividing a measure of time. A 'Bar' contains more structure than a
-- 'NoteSequence' but ultimately must be translated to a 'NoteSequence' to be of any use. The 'Bar'
-- data type is used within the 'Composition' data type to compose music, whereas the 'NoteSequence'
-- data type is used to sequence the sounds that make music onto a recorded track of audio data.
data Bar leaf
  = BarLeaf   !leaf
  | BarBranch !(Boxed.Vector (Bar leaf))
  deriving (Eq, Functor)

instance Semigroup (Bar leaf) where
  a <> b = case a of
    BarLeaf{} -> case b of
      BarLeaf{}   -> BarBranch $ Boxed.fromList [a, b]
      BarBranch b -> if Boxed.null b then a else BarBranch $ Boxed.cons a b
    BarBranch a -> if Boxed.null a then b else case b of
      BarLeaf{}   -> BarBranch $ Boxed.snoc a b
      BarBranch b -> BarBranch $ a <> b

instance Monoid (Bar leaf) where
  mempty = BarBranch Boxed.empty
  mappend = (<>)

-- | Construct a lazy list in reverse order (so you can 'Prelude.foldr' over it in forward
-- order). The result of this function evaluation is intended to be passed to other intermediate
-- computations before producing a final 'NoteSequence' data structure.
sequenceBar :: Moment -> Duration -> Bar note -> [(Moment, Duration, note)]
sequenceBar t0 dt0 msur = loop dt0 (t0, msur) [] where
  loop dt0 (t0, subdiv) list = case subdiv of
    BarLeaf  note -> (t0, dt0, note) : list
    BarBranch vec -> if Boxed.null vec then mempty else
      let dt = dt0 / realToFrac (Boxed.length vec) in
      foldr (loop dt) list $ zip (iterate (+ dt) t0) (Boxed.toList vec)

----------------------------------------------------------------------------------------------------

-- | A mapping from 'ProcGen.Types.Moment's in time to @note@s. Essentially this is just a map data
-- structure that instantiates 'Data.Semigroup.Semigroup' and 'Data.Monoid.Monoid' such that the
-- append @('Data.Semigroup.<>')@ function performs the right-biased union of the maps, where
-- right-bias meaning the @note@ on the right of the @('Data.Semigroup.<>')@ operator overwrites the
-- @note on the left of the operator if the two operators appear in the exact same
-- 'ProcGen.Types.Moment' in time. Use 'playNoteSequence' to convert a 'Bar' to a 'NoteSequence'.
newtype NoteSequence note = NoteSequence (Map.Map Moment [note])
  deriving (Eq, Functor)

instance Semigroup (NoteSequence note) where
  NoteSequence a <> NoteSequence b = NoteSequence $ Map.unionWith (++) a b

instance Monoid (NoteSequence note) where
  mempty = NoteSequence Map.empty
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
    PlayedNote{} -> (,) t1 $ n1
      { playedTied      = n2
      , playedDuration  = playedDuration n2 + t2 - t1
      , playedNoteValue = let unchanged = playedNoteValue n1 in case n2 of
          RestNote     -> unchanged
          PlayedNote{} ->
            let (ToneID key1 tags1) = playedNoteValue n1
                (ToneID key2 _    ) = playedNoteValue n2
            in  case key1 of
                  KeyTone a -> case key2 of
                    KeyTone b -> ToneID (TiedNote TieNotes a b) tags1
                    _         -> unchanged
                  _         -> unchanged
      }
  makeTied = fmap (foldl1 tie) . IMap.elems

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
-- 'Bar' is constructed. It is only after evaluating the 'Composition' using 'runComposition'
-- that you can extract the 'Bar' and convert it to a timed sequence of notes using
-- @('setNoteDurations' . 'sequenceBar')@.
newtype Composition note a = Composition (StateT (CompositionState note) (TFRandT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

data CompositionState note
  = CompositionState
    { theComposeTieID        :: !Int
    , theComposeNoteCount    :: !Int
    , theComposeKeySignature :: [ToneIndex]
    , theComposeNotes        :: [Bar note]
    }

instance MonadState (CompositionState note) (Composition note) where
  state = Composition . state

instance Semigroup a => Semigroup (Composition note a) where { a <> b = (<>) <$> a <*> b; }

instance Monoid a => Monoid (Composition note a) where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b

instance MonadRandom (Composition note) where
  getRandomR  = Composition . lift . getRandomR
  getRandom   = Composition $ lift getRandom
  getRandomRs = Composition . lift . getRandomRs
  getRandoms  = Composition $ lift getRandoms

emptyCompositionState :: CompositionState note
emptyCompositionState = CompositionState
  { theComposeTieID        = 0
  , theComposeNoteCount    = 0
  , theComposeKeySignature = []
  , theComposeNotes        = []
  }

composeTieID :: Lens' (CompositionState note) Int
composeTieID = lens theComposeTieID $ \ a b -> a{ theComposeTieID = b }

composeKeySignature :: Lens' (CompositionState note) [ToneIndex]
composeKeySignature = lens theComposeKeySignature $ \ a b -> a{ theComposeKeySignature = b }

composeNotes :: Lens' (CompositionState note) [Bar note]
composeNotes = lens theComposeNotes $ \ a b -> a{ theComposeNotes = b }

composeNoteCount :: Lens' (CompositionState note) Int
composeNoteCount = lens theComposeNoteCount $ \ a b -> a{ theComposeNoteCount = b }

----------------------------------------------------------------------------------------------------

class ScorableNote n where
  -- | Convert some value to a 'Note' so it can be played by 'note'. The unit @()@ data type
  -- instantiates this class such that @()@ can be used to indicate a 'RestNote'.
  toNote :: n -> Composition note ScoredNote

instance ScorableNote ()         where { toNote = const $ return ScoredRestNote; }
instance ScorableNote ScoredNote where { toNote = return; }

-- | Evaluate a 'Composition' function automatically seeding a new random number generator using
-- 'Control.Random.TF.Init.initTFGen'.
evalComposition :: Composition note a -> IO a
evalComposition (Composition f) = liftM fst $
  seedIORunTFRandT $ evalStateT f emptyCompositionState 

randGenComposition :: Composition note a -> TFGen -> IO (a, TFGen)
randGenComposition (Composition f) = runTFRandT (evalStateT f emptyCompositionState)

-- | Play a note
note :: ScorableNote n => n -> Composition ScoredNote ()
note n = toNote n >>= (%=) composeNotes . (:) . BarLeaf >> composeNoteCount += 1

-- | Leave a brief silent gap in the current 'Bar' (a gap in the sub-division of the current
-- interval).
rest :: Composition ScoredNote ()
rest = note ScoredRestNote

-- | Sieze the current time 'Interval' of the current 'Bar', and begin sub-dividing it with every
-- note played. The 'Composition' state does not contain timing information, timing is applied using
-- 'playNotSequence', however the information on how time is sub-divided is maintained in the 'Composition' state.
--
-- After a 'Composition' function is evaluated by 'evalComposition', suppose you pass the top-level
-- 'Bar' passed to 'playNoteSequence' using a duration of 4.0 seconds for the top-level
-- interval. When the notes are sequenced, the 'quick' function will subdivide this 4.0 second
-- interval such that if you play 4 'note's, each note is played for a duration of 1.0 seconds.
--
-- If instead of playing a 'note' you play a nested call to 'quick' the 1.0 second interval will be
-- further sub-divided. Within this nested 'quick' if two 'note's are played, each note is played
-- for a duration of 0.5 seconds.
quick :: Composition note void -> Composition note (Bar note)
quick = fmap snd . quick'

-- | Like 'quick' but does not throw away the value of type @a@ that was returned by the
-- 'Composition' function parameter that was evaluated.
quick' :: Composition note a -> Composition note (a, Bar note)
quick' subcomposition = do
  oldnotes <- use composeNotes     <* (composeNotes     .= [])
  oldcount <- use composeNoteCount <* (composeNoteCount .= 0)
  a <- subcomposition
  notes    <- use composeNotes
  count    <- use composeNoteCount
  let mkvec count elems = Boxed.create $ do
        vec <- Mutable.new count
        mapM_ (uncurry $ Mutable.write vec) $ zip (iterate (subtract 1) (count - 1)) elems
        return vec
  let subdiv = BarBranch $ mkvec count notes
  composeNotes     .= subdiv : oldnotes
  composeNoteCount .= oldcount + 1
  return (a, subdiv)

-- | Remove the current 'Bar' and replace it with a new empty 'Bar', return the previous 'Bar'.
nextBar :: Composition note (Bar note)
nextBar = do
  notes <- use composeNotes
  composeNotes .= mempty
  case notes of
    []  -> return mempty
    [a] -> return a
    ax  -> return $ BarBranch $ Boxed.fromList ax

-- | Play a 'note' that will be tied to another 'note' at some point in the future. The tied note is
-- held for a time until the future 'untie'd note is reached.
tieNote :: ScorableNote n => n -> Composition ScoredNote (NoteReference, ScoredNote)
tieNote = toNote >=> \ case
  n@ScoredNote{}  -> do
    i <- composeTieID += 1 >> use composeTieID
    composeNotes %= (:) (BarLeaf n{ scoredTiedID = NoteReference i })
    return (NoteReference i, n)
  ScoredRestNote -> do
    return (untied, ScoredRestNote)

-- | Shorthand for 'tieNote' returning only the 'NoteReference' and not the 'Note' constructed with
-- it.
tie :: ScorableNote n => n -> Composition ScoredNote NoteReference
tie = fmap fst . tieNote

-- | Stop playing a tied 'note'. Supply the 'NoteReference' returend by a previous call to 'tie',
-- pass the note value to which the note must be tied as @note1@ (passing 'RestNote' means to tie
-- the same note that initiated the 'tie'). The value @note2@ can be played with the note that is
-- being united at the time interval it is untied.
untie :: ScorableNote note => NoteReference -> note -> Composition ScoredNote ()
untie ref = toNote >=> \ case
  ScoredRestNote -> return ()
  n@ScoredNote{} -> note n{ scoredTiedID = ref }


