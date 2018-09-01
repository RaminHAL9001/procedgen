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
    Note(..), makeNote, NoteReference, untied, NoteValue(..), noteValue, Strength(..),
    -- * Arranging Notes
    Measure(..), makeMeasure,
    SubDiv(..), RoleNotes(..), roleNote, sequenceMeasure, setNoteDurations,
    -- * Composing Music
    Composition, newComposition, measure,
    notes, rest, tie, addNote, play, playNotes, playTie, score,
    module ProcGen.Arbitrary,
    module Control.Monad.State.Class,
  ) where

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Music.KeyFreq88

import           Control.Lens
import           Control.Monad.Primitive
import           Control.Monad.State
import           Control.Monad.State.Class

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
-- only in 'Map.Map' data structures within the 'Measure' data type.
data Interval note = Interval !Duration !note
  deriving (Eq, Ord, Show)

-- | A 'Composition' is constructed of a list of 'Note's each played at a point in time.
data Note
  = RestNote -- ^ indicates that nothing is played for the duration of the 'SubDiv'.
    -- ^ This must refer to a note that was played at an earlier point in the composition.
  | PlayedNote
    { playedNoteID    :: !NoteReference
    , playedTiedID    :: !NoteReference
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
    { playedNoteID    = untied -- ^ allow 'addNote' to initialize this to the correct value
    , playedTiedID    = tied
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
    -- constructed by a 'keySigFreqTable' for whatever key signature a given measure is played in.
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

-- | A data structure for sub-dividing a measure of time.
data SubDiv leaf
  = SubDivLeaf   !leaf
  | SubDivBranch !(Boxed.Vector (SubDiv leaf))
  deriving Functor

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
  deriving Functor

makeMeasure :: Measure leaf
makeMeasure = Measure
  { playMetaOffset = 0.0
  , playMetaCutoff = 1.0
  , playNoteTree   = SubDivBranch Boxed.empty
  }

----------------------------------------------------------------------------------------------------

-- | The notes played by a single 'Role' at each point in time. The time the note played is defined
-- by the index of the 'Map.Map', where each measure contains up to @measureTimeDiv@ elements.
newtype RoleNotes leaf = RoleNotes (Map.Map Moment leaf)
  deriving (Eq, Functor)

instance Semigroup (RoleNotes leaf) where
  (RoleNotes a) <> (RoleNotes b) = RoleNotes (Map.union b a)

instance Monoid (RoleNotes leaf) where
  mempty = RoleNotes mempty
  mappend = (<>)

roleNote :: Moment -> Duration -> leaf -> RoleNotes (Duration, leaf)
roleNote t dt = RoleNotes . Map.singleton t . (,) dt

-- | A 'Measure' sub-divides the given initial 'ProcGen.Types.Duration' into several sub-intervals
-- associated with the leaf elements. This function converts a 'Measure' into a mapping from the
-- start time to the @('ProcGen.Types.Duration', leaf)@ pair. When the @leaf@ type is unified with
-- 'Note', it is helpful to evaluate the resulting 'RoleNotes' with 'setNoteDurations'.
sequenceMeasure :: Moment -> Duration -> Measure leaf -> RoleNotes (Duration, leaf)
sequenceMeasure t0 dt0 msur = loop dt0 mempty (t0 + playMetaOffset msur, playNoteTree msur) where
  loop dt0 map (t0, subdiv) = if t0 >= playMetaCutoff msur then map else case subdiv of
    SubDivLeaf  note -> map <> roleNote t0 dt0 note
    SubDivBranch vec -> if Boxed.null vec then mempty else
      let dt = dt0 / realToFrac (Boxed.length vec) in
      foldl (loop dt) map $ zip (iterate (+ dt) t0) (Boxed.toList vec)

-- | For a 'RoleNotes' containing a tuple @('Duration', 'Note')@ pair as what is constructed
-- by the 'sequenceMeasure' function, this sets the 'ProcGen.Types.Duration' value in the
-- 'Prelude.fst' of the tuple as the 'playedDuration' of each 'Note' in the 'Prelude.snd' of
-- the tuple.
setNoteDurations :: RoleNotes (Duration, Note) -> RoleNotes (Interval Note)
setNoteDurations = fmap $ uncurry Interval

----------------------------------------------------------------------------------------------------

-- | This function type allows you to construct a musical 'Composition'.
newtype ComposeT m a = ComposeT (StateT (Composition m) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | This is a data type for constructing a musical composition. It can be evaluated purely in the
-- 'Control.Monad.ST.ST' monad, or in the 'IO' monad.
data Composition m
  = Composition
    { theCompositionNoteCount    :: !Int
    , theCompositionNotes        :: !(Mutable.MVector (PrimState m) Note)
    , theCompositionRandGen      :: !TFGen
    , theCompositionMeasureTime  :: !Duration
    , theCompositionMeasureCount :: !Int
    , theCompositionMeasures     :: !(Mutable.MVector (PrimState m) (Measure NoteReference))
    , theCompositionDivSize      :: !Int
    , theCompositionCurrentDiv   :: [NoteReference]
    }

instance Monad m => MonadState (Composition m) (ComposeT m) where { state = ComposeT . state; }

instance MonadTrans ComposeT where { lift = ComposeT . lift; }

instance (Semigroup a, Monad m) => Semigroup (ComposeT m a) where { (<>) a b = (<>) <$> a <*> b; }

instance (Monoid a, Monad m) => Monoid (ComposeT m a) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b

-- | Construct a new composition in either the @IO@ or 'Control.Monad.ST.ST' monad. Generate a
-- 'TFGen' with 'ProcGen.Arbitrary.initTGGen', or 'ProcGen.Arbitrary.tfGen'.
newComposition :: PrimMonad m => TFGen -> m (Composition m)
newComposition gen = do
  notes    <- Mutable.new 256
  measures <- Mutable.new 64
  return Composition
    { theCompositionNoteCount    = 0
    , theCompositionNotes        = notes
    , theCompositionRandGen      = gen
    , theCompositionMeasureTime  = 4.0
    , theCompositionMeasureCount = 0
    , theCompositionMeasures     = measures
    , theCompositionDivSize      = 0
    , theCompositionCurrentDiv   = []
    }

-- TODO: newCompositionIO, newCompositionPure, runComposeIO, runComposePure

-- not for export
compositionNoteCount :: Lens' (Composition m) Int
compositionNoteCount = lens theCompositionNoteCount $ \ a b -> a{ theCompositionNoteCount = b }

-- not for export
compositionNotes :: Lens' (Composition m) (Mutable.MVector (PrimState m) Note)
compositionNotes = lens theCompositionNotes $ \ a b -> a{ theCompositionNotes = b }

-- not for export
compositionRandGen :: Lens' (Composition m) TFGen
compositionRandGen = lens theCompositionRandGen $ \ a b -> a{ theCompositionRandGen = b }

-- not for export
compositionMeasureTime :: Lens' (Composition m) Duration
compositionMeasureTime = lens theCompositionMeasureTime $ \ a b -> a{ theCompositionMeasureTime = b }

-- not for export
compositionMeasureCount :: Lens' (Composition m) Int
compositionMeasureCount = lens theCompositionMeasureCount $ \ a b -> a{ theCompositionMeasureCount = b }

-- not for export
compositionMeasures :: Lens' (Composition m) (Mutable.MVector (PrimState m) (Measure NoteReference))
compositionMeasures = lens theCompositionMeasures $ \ a b -> a{ theCompositionMeasures = b }

-- not for export
compositionDivSize :: Lens' (Composition m) Int
compositionDivSize = lens theCompositionDivSize $ \ a b -> a{ theCompositionDivSize = b }

-- not for export
compositionCurrentDiv :: Lens' (Composition m) [NoteReference]
compositionCurrentDiv = lens theCompositionCurrentDiv $ \ a b -> a{ theCompositionCurrentDiv = b }

-- | Construct and return a 'Measure'.
--
-- @
-- 'runComposeIO' $ do
--     a     <- 'notes' [1, 5, 8]
--     b     <- 'chord' [0, 3, 4]
--     intro <- 'measure' $ do
--                  'measure' $ 'play' a >> 'rest'
--                  'measure' $ 'play' b >> play a
--     'score' intro
-- @
measure :: Monad m => ComposeT m () -> ComposeT m (Measure NoteReference)
measure compose = do
  oldlist <- use compositionCurrentDiv
  oldsize <- use compositionDivSize
  oldtime <- use compositionMeasureTime
  compositionCurrentDiv  .= []
  compositionDivSize     .= 0
  compositionMeasureTime %= (/ 2.0)
  compose
  newlist <- fmap SubDivLeaf <$> use compositionCurrentDiv
  newsize <- use compositionDivSize
  compositionCurrentDiv  .= oldlist
  compositionDivSize     .= oldsize
  compositionMeasureTime .= oldtime
  return makeMeasure
    { playNoteTree = SubDivBranch $ Boxed.create $ do
        vec <- Mutable.new newsize
        mapM_ (uncurry $ Mutable.write vec) $ zip (iterate (subtract 1) $ newsize - 1) newlist
        return vec
    }

-- TODO: extract this function, along with the 'theCompositionNoteCount' and 'theCompositionNotes'
-- elements in the 'Composition' data type into their own data type in a separate module, because
-- these fields and this function are a common mutable data structure used: a mutable buffer of
-- items that grows exponentially and automatically as elements are added to it.
incrementMutable
  :: PrimMonad m
  => Lens' (Composition m) (Mutable.MVector (PrimState m) elem)
  -> Lens' (Composition m) Int
  -> ComposeT m (Int, Mutable.MVector (PrimState m) elem)
incrementMutable vector nelems = do
  i       <- use nelems
  nelems += 1
  reqsize <- use nelems
  vec     <- use vector
  let allocsize = Mutable.length vec
  vec     <- if reqsize < allocsize then return vec else do
    vec <- lift $ Mutable.grow vec $ head $ dropWhile (<= reqsize) $ iterate (* 2) allocsize
    vector .= vec
    return vec
  return (i, vec)

-- | Register a pre-constructed note. 
addNote :: PrimMonad m => Note -> ComposeT m NoteReference
addNote note = do
  (i, vec) <- incrementMutable compositionNotes compositionNoteCount
  let noteID = NoteReference i
  lift $ Mutable.write vec i $ note{ playedNoteID = noteID }
  return noteID

-- | Construct an arbitrary note, unconstrained by the key signature. But don't play the constructed
-- note yet. Instead a reference to a note is returned which you can play, and if necessary, you can
-- tie it to other notes. To construct a note that you never intend to tie and just play it
-- immediately, use 'playNote' instead.
notes :: PrimMonad m => Strength -> [KeyIndex] -> ComposeT m NoteReference
notes strength = addNote . makeNote untied strength
  
-- | Construct a rest.
rest :: PrimMonad m => ComposeT m NoteReference
rest = addNote RestNote

-- | Construct a note tied to the given 'NoteReference'.
tie :: PrimMonad m => Strength -> [KeyIndex] -> NoteReference -> ComposeT m NoteReference
tie strength ns noteID@(NoteReference i) = do
  tiedID <- addNote $ makeNote noteID strength ns
  vec <- use compositionNotes
  let untieLoop noteID@(NoteReference i) = unless (noteID == untied) $ do
        prev <- Mutable.read vec i
        when (playedTiedID prev == noteID) $ do
          Mutable.write vec i $ prev{ playedTiedID = untied }
          untieLoop $ playedTiedID prev
  lift $ do
    note <- Mutable.read vec i
    untieLoop $ playedTiedID note
    Mutable.write vec i $ note{ playedTiedID = tiedID }
  return tiedID

-- | Record a 'NoteReference' into the current 'Measure'. Only notes that are played, and not merely
-- constructed by 'notes' or 'addNotes', will be converted to sound in the synthesizer.
play :: Monad m => NoteReference -> ComposeT m ()
play ref = compositionCurrentDiv %= (ref :) >> compositionDivSize  += 1

-- | Construct a 'NoteReference' using 'notes' and immediately pass the reference to 'play' to have
-- it recorded. 
playNotes :: PrimMonad m => Strength -> [KeyIndex] -> ComposeT m NoteReference
playNotes s n = notes s n >>= \ noteID -> play noteID >> return noteID

-- | Construct a 'NoteReference' using 'tie' and immediately pass the referece to 'play' to have it
-- recorded, then tie the note to a previouly 'play'ed 'NoteReference'.
playTie :: PrimMonad m => Strength -> [KeyIndex] -> NoteReference -> ComposeT m NoteReference
playTie s n k = tie s n k >>= \ noteID -> play noteID >> return noteID

-- | Once the 'measure' function returns it constructs a 'Measure' but does not record it to the
-- 'Composition'. This function records the 'Measure' into the 'Composition' so that the synthesizer
-- can actually convert it to sound. Obviously you may 'score' the same 'Measure' as many times as
-- you like, just be aware that people tend to find the same thing repeated again and again too many
-- times to be boring.
score :: PrimMonad m => Measure NoteReference -> ComposeT m ()
score mesr = do
  (i, vec) <- incrementMutable compositionMeasures compositionMeasureCount
  lift $ Mutable.write vec i mesr
