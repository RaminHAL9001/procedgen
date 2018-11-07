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
    ScoredNote(..), scoreTone, scoreDrum,
    PlayedTone, PlayedDrum, PlayedNote(..), playScoredNote, getTiedNotes,
    NoteReference, untied,
    Strength(..),  strengthToAmplitude,
    -- * Arranging Notes
    Bar, PlayedRole(..), sequenceBar, tieSequencedNotes,
    playedRoleInstrument, playedRoleSequence, nextBar,
    NoteSequence(..), listNoteSequence, playNoteSequence, getPlayedRoles,
    -- * Composing Music for a Single Role
    Notation, evalNotation, randGenNotation, notationKeySignature,
    ScorableNote(..), note, rest, intTone, playNote, quick, tieNote, tie, untie,
    Composition, CompositionState(..), runCompositionTFGen, emptyComposition,
    instrument, composeNotes, composedDrums,
    exampleComposition,
    module ProcGen.Arbitrary,
    module Control.Monad.State.Class,
  ) where

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Music.KeyFreq88
import           ProcGen.Music.SoundFont

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.State.Class
import           Control.Monad.Trans.Random.Lazy

import           Data.Maybe
import qualified Data.IntMap               as IMap
import qualified Data.Map                  as Map
import           Data.Semigroup
import qualified Data.Vector.Mutable       as Mutable
import qualified Data.Vector               as Boxed

import           Text.Printf

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

type ScoredTone = ScoredNote ToneValue
type ScoredDrum = ScoredNote DrumValue

-- | 'Notation' constructs a list of 'ScoredNote's each played at a point in time. A 'ScoredNote' is
-- different from a 'PlayedNote' in that a 'ScoredNote' is an element of the 'Notation' type whereas
-- a 'PlayedNote' is an instruction to a synthesizer to play a particular note.
data ScoredNote value
  = ScoredNoteRest -- ^ indicates that nothing is played for the duration of the 'Bar'.
    -- ^ This must refer to a note that was played at an earlier point in the composition.
  | ScoredNote
    { scoredNoteTiedID    :: !NoteReference
      -- ^ Set to 'untied' if this note is not tied. If not 'untied' (if it is tied) it must refer
      -- to the 'playedNoteID' of 'Note' defined with the 'tieNote' constructor.
    , scoredNoteStrength  :: !Strength
    , scoredNoteValue     :: !(NoteID value)
    }
  deriving (Eq, Ord, Show)

type PlayedTone = PlayedNote ToneValue
type PlayedDrum = PlayedNote DrumValue

-- | A 'ScoredNote' that has been sequenced using 'sequenceBar'. This data type is basically an
-- instruction to a synthesizer on what note to play.
data PlayedNote value
  = RestNote
  | PlayedNote
    { playedDuration  :: !Duration
    , playedStrength  :: !Strength
    , playedNoteValue :: !(NoteID value)
    , playedTied      :: !(PlayedNote value)
    }

instance HasTimeWindow (PlayedNote any) Moment where
  timeWindow = \ case
    RestNote -> Nothing
    PlayedNote{ playedDuration=dt } -> Just TimeWindow{ timeStart=0, timeEnd=dt }

instance Show value => Show (PlayedNote value) where
  show = \ case
    RestNote       -> "rest"
    n@PlayedNote{} -> do
      let show_dt = printf "%5f %s" (playedDuration n) (show $ playedStrength n)
      let ident   = replicate (22 - length show_dt) ' '
      (show_dt ++) . (ident ++) . shows (playedNoteValue n) $ case playedTied n of
        RestNote -> ""
        _        -> " [tied]"

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
getTiedNotes :: PlayedNote any -> [PlayedNote any]
getTiedNotes = \ case
  RestNote -> []
  note1    -> case playedTied note1 of
    RestNote -> [note1]
    note2    -> note1 : getTiedNotes note2

scoreNote
  :: (a -> [a] -> value)
  -> NoteReference -> [PlayNoteTag] -> Strength -> [a] -> ScoredNote value
scoreNote constr tied tags strength = \ case
  []   -> ScoredNoteRest
  a:ax -> ScoredNote
    { scoredNoteTiedID   = tied
    , scoredNoteStrength = strength
    , scoredNoteValue    = NoteID (NoteKey $ constr a ax) (playNoteTagSet tags)
    }

-- | Construct a note from a 'Strength' and zero or more 'ProcGen.Music.KeyFreq88.KeyIndex' values
-- which refer to notes on an 88-key piano keyboar.d
scoreTone :: NoteReference -> [PlayNoteTag] -> Strength -> [KeyIndex] -> ScoredTone
scoreTone = scoreNote noteValue

scoreDrum :: NoteReference -> [PlayNoteTag] -> Strength -> [DrumIndex] -> ScoredDrum
scoreDrum = scoreNote drumValue

-- | Convert a 'ScoredNote' to a 'PlayedNote'.
playScoredNote :: Duration -> ScoredNote any -> PlayedNote any
playScoredNote dt = \ case
  ScoredNoteRest -> RestNote
  note           -> PlayedNote
    { playedDuration  = dt
    , playedStrength  = scoredNoteStrength note
    , playedNoteValue = scoredNoteValue note
    , playedTied      = RestNote
    }

----------------------------------------------------------------------------------------------------

-- | Every note written by 'Notation' has a unique index value or "reference". The 'Notation' keeps
-- track of each unique note played in order to evaluate "tied notes" which are notes are held for
-- an amount of time, sometimes sliding (gradually changing the sound'S base frequency) into another
-- note.
newtype NoteReference = NoteReference Int deriving (Eq, Ord)

instance Show NoteReference where
  show ref@(NoteReference i) = if ref == untied then "untied" else "(tie "++show i++")"

-- | A 'Note' that is not tied.
untied :: NoteReference
untied = NoteReference minBound

----------------------------------------------------------------------------------------------------

-- | A data structure for sub-dividing a measure of time. A 'Bar' contains more structure than a
-- 'NoteSequence' but ultimately must be translated to a 'NoteSequence' to be of any use. The 'Bar'
-- data type is constructed during evaluation of 'Notation' functions to compose music, whereas the
-- 'NoteSequence' data type is used to sequence the sounds that make music onto a recorded track of
-- audio data.
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

instance HasTimeWindow note Moment => HasTimeWindow (NoteSequence note) Moment where
  timeWindow =
    ( listNoteSequence >=> \ (t0, notes) ->
      twShift t0 <$> (notes >>= maybeToList . timeWindow)
    ) >>> \ case
      []   -> Nothing
      a:ax -> Just $ foldl twMinBounds a ax

instance Show note => Show (NoteSequence note) where
  show (NoteSequence map) = do
    (t, notes) <- Map.assocs map
    case notes of
      []       -> ""
      n0:notes -> do
        let show_t = show t
        let inden  = replicate (length show_t + 1) ' '
        unlines $ (show_t ++ ' ':show n0)
                : (notes >>= \ n -> [inden ++ show n])

listNoteSequence :: NoteSequence note -> [(Moment, [note])]
listNoteSequence (NoteSequence map) = Map.assocs map

-- | A 'Bar' sub-divides the given initial 'ProcGen.Types.Duration' into several sub-intervals
-- associated with the leaf elements. This function converts a 'Measure' into a mapping from the
-- start time to the @('ProcGen.Types.Duration', leaf)@ pair. When the @leaf@ type is unified with
-- 'Note', it is helpful to evaluate the resulting 'PlayedRole' with 'setNoteDurations'.
playNoteSequence :: Moment -> Duration -> Bar ScoredTone -> NoteSequence PlayedTone
playNoteSequence t0 dt0 = NoteSequence
  . foldr (uncurry $ Map.insertWith (++)) Map.empty
  . fmap (fmap pure) . tieSequencedNotes . sequenceBar t0 dt0

-- | A 'Prelude.foldr' over the result of 'sequenceBar' in which 'PlayedNotes' that share the same
-- 'NoteReference' in their 'scoredTiedID' are "tied" together, meaning all tied notes are linked
-- together into a single 'PlayedNote' structure where each next tied note is set in the
-- 'playedTied' field of the 'PlayedNote' data structure. Keep in mind that the tied notes are
-- appended to the result list so this function causes notes to be listed out of order. Use
-- 'playNoteSequence' to re-order the elements in this list.
tieSequencedNotes :: [(Moment, Duration, ScoredTone)] -> [(Moment, PlayedTone)]
tieSequencedNotes = uncurry (flip (++)) . fmap makeTied . foldr f ([], IMap.empty) where
  f (t, dt, scor) (list, table) = case scor of
    ScoredNoteRest -> (list, table)
    scor@ScoredNote{scoredNoteTiedID=tiedTo} ->
      let (NoteReference i) = tiedTo
          play = (t, playScoredNote dt scor)
      in  if tiedTo == untied then (play:list, table) else
            (list, IMap.alter (Just . maybe [play] (play :)) i table)
  tie (t2, n2) (t1, n1) = case n1 of
    RestNote     -> (t2, n2)
    PlayedNote{} -> (,) t1 $ n1
      { playedTied      = n2
      , playedDuration  = playedDuration n2 + t2 - t1
      , playedNoteValue = let unchanged = playedNoteValue n1 in case n2 of
          RestNote     -> unchanged
          PlayedNote{} ->
            let (NoteID key1 tags1) = playedNoteValue n1
                (NoteID key2 _    ) = playedNoteValue n2
            in  case key1 of
                  NoteKey a -> case key2 of
                    NoteKey b -> NoteID (NoteTied a b) tags1
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

instance HasTimeWindow (PlayedRole (PlayedNote any)) Moment where
  timeWindow = timeWindow . thePlayedRoleSequence

instance HasTimeWindow [PlayedRole (PlayedNote any)] Moment where
  timeWindow = twMinBoundsAll . (>>= (maybeToList . timeWindow))

instance Show note => Show (PlayedRole note) where
  show n = show (thePlayedRoleSequence n) ++ '\n' : show (thePlayedRoleSequence n)

playedRoleInstrument :: Lens' (PlayedRole note) InstrumentID
playedRoleInstrument = lens thePlayedRoleInstrument $ \ a b -> a{ thePlayedRoleInstrument = b }

playedRoleSequence :: Lens' (PlayedRole note) (NoteSequence note)
playedRoleSequence = lens thePlayedRoleSequence $ \ a b -> a{ thePlayedRoleSequence = b }

----------------------------------------------------------------------------------------------------

-- | This is a monadic function type designed so you can construct the 'Bar's of an individual role
-- for a piece of music using @do@ notation. Functions of this data type such as 'note', 'rest',
-- 'quick', 'tie', and 'untie'.
--
-- One aspect that may be confusing about the 'Notation' function type is that the state of the
-- 'Notation' does not keep track of time at all, nor does it track what role is being
-- played. Instead, you evaluate a 'Notation' function to define various musical motifs which you
-- can then use to compose a larger piece of music in the 'Composition' function type.
--
-- The type that binds to @value@ should be either 'ProcGen.Music.SoundFont.ToneValue' or
-- 'ProcGen.Music.SoundFont.DrumValue'.
newtype Notation value a = Notation (StateT (NotationState value) (TFRandT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

data NotationState value
  = NotationState
    { theNotationTieID        :: !Int
    , theNotationNoteCount    :: !Int
    , theNotationKeySignature :: [ToneIndex]
    , theNotationNotes        :: [Bar (ScoredNote value)]
    }

instance MonadState (NotationState any) (Notation any) where
  state = Notation . state

instance Semigroup a => Semigroup (Notation any a) where { a <> b = (<>) <$> a <*> b; }

instance Monoid a => Monoid (Notation any a) where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b

instance MonadRandom (Notation any) where
  getRandomR  = Notation . lift . getRandomR
  getRandom   = Notation $ lift getRandom
  getRandomRs = Notation . lift . getRandomRs
  getRandoms  = Notation $ lift getRandoms

instance MonadSplit TFGen (Notation any) where
  getSplit = Notation $ lift getSplit

emptyNotationState :: NotationState any
emptyNotationState = NotationState
  { theNotationTieID        = 0
  , theNotationNoteCount    = 0
  , theNotationKeySignature = []
  , theNotationNotes        = []
  }

notationTieID :: Lens' (NotationState any) Int
notationTieID = lens theNotationTieID $ \ a b -> a{ theNotationTieID = b }

notationKeySignature :: Lens' (NotationState any) [ToneIndex]
notationKeySignature = lens theNotationKeySignature $ \ a b -> a{ theNotationKeySignature = b }

notationNotes :: Lens' (NotationState value) [Bar (ScoredNote value)]
notationNotes = lens theNotationNotes $ \ a b -> a{ theNotationNotes = b }

notationNoteCount :: Lens' (NotationState any) Int
notationNoteCount = lens theNotationNoteCount $ \ a b -> a{ theNotationNoteCount = b }

----------------------------------------------------------------------------------------------------

class ScorableNote n value where
  -- | Convert some value to a 'Note' so it can be played by 'note'. The unit @()@ data type
  -- instantiates this class such that @()@ can be used to indicate a 'RestNote'.
  toNote :: n -> Notation value (ScoredNote value)

instance ScorableNote ()          DrumValue where { toNote = const $ return ScoredNoteRest; }
instance ScorableNote [DrumIndex] DrumValue where { toNote = return . scoreDrum untied [] Moderare; }
instance ScorableNote ()          ToneValue where { toNote = const $ return ScoredNoteRest; }
instance ScorableNote ScoredTone  ToneValue where { toNote = return; }
instance ScorableNote [KeyIndex]  ToneValue where { toNote = return . scoreTone untied [] Moderare; }
instance ScorableNote [Int]       ToneValue where { toNote = toNote . fmap keyIndex; }

instance ScorableNote ToneValue   ToneValue where
  toNote n = pure ScoredNote
    { scoredNoteTiedID   = untied
    , scoredNoteValue    = NoteID (NoteKey n) $ playNoteTagSet []
    , scoredNoteStrength = Moderare
    }

instance ScorableNote DrumValue   DrumValue where
  toNote n = pure ScoredNote
    { scoredNoteTiedID   = untied
    , scoredNoteValue    = NoteID (NoteKey n) $ playNoteTagSet []
    , scoredNoteStrength = Moderare
    }

intTone :: [Int] -> Notation ToneValue ScoredTone
intTone = toNote >=> \ n -> playNote n >> return n

-- | Evaluate a 'Notation' function automatically seeding a new random number generator using
-- 'Control.Random.TF.Init.initTFGen'.
evalNotation :: Notation note a -> IO a
evalNotation (Notation f) = liftM fst $
  seedIORunTFRandT $ evalStateT f emptyNotationState 

randGenNotation :: Notation note a -> TFGen -> IO (a, TFGen)
randGenNotation (Notation f) = runTFRandT (evalStateT f emptyNotationState)

-- | If you already have a 'ScoredNote' of some type (like a 'ScoredTone' or a 'ScoredDrum'), you
-- can play it again using 'playNote'.
playNote :: ScoredNote value -> Notation value ()
playNote note = notationNotes %= (:) (BarLeaf note) >> notationNoteCount += 1

-- | Play a note
note :: ScorableNote note value => note -> Notation value ()
note n = toNote n >>= playNote

-- | Leave a brief silent gap in the current 'Bar' (a gap in the sub-division of the current
-- interval).
rest :: Notation value ()
rest = playNote ScoredNoteRest

-- | Sieze the current time 'Interval' of the current 'Bar', and begin sub-dividing it with every
-- note played. The 'Notation' state does not contain timing information, timing is applied using
-- 'playNotSequence', however the information on how time is sub-divided is maintained in the
-- 'Notation' state.
--
-- After a 'Notation' function is evaluated by 'evalNotation', suppose you pass the top-level 'Bar'
-- passed to 'playNoteSequence' using a duration of 4.0 seconds for the top-level interval. When the
-- notes are sequenced, the 'quick' function will subdivide this 4.0 second interval such that if
-- you evaluate the 'note' function 4 times, each 'note' is played for a duration of 1.0 seconds.
--
-- If instead of evaluating 'note' you evaluate a nested call to 'quick' the 1.0 second interval
-- will be further sub-divided. Within this nested 'quick' if two 'note's are played, each note is
-- played for a duration of 0.5 seconds.
quick :: Notation value void -> Notation value (Bar (ScoredNote value))
quick = fmap snd . quick'

-- | Like 'quick' but does not throw away the value of type @a@ that was returned by the
-- 'Notation' function parameter that was evaluated.
quick' :: Notation value a -> Notation value (a, Bar (ScoredNote value))
quick' subcomposition = do
  oldnotes <- use notationNotes     <* (notationNotes     .= [])
  oldcount <- use notationNoteCount <* (notationNoteCount .= 0)
  a <- subcomposition
  notes    <- use notationNotes
  count    <- use notationNoteCount
  let mkvec count elems = Boxed.create $ do
        vec <- Mutable.new count
        mapM_ (uncurry $ Mutable.write vec) $ zip (iterate (subtract 1) (count - 1)) elems
        return vec
  let subdiv = BarBranch $ mkvec count notes
  notationNotes     .= subdiv : oldnotes
  notationNoteCount .= oldcount + 1
  return (a, subdiv)

-- | Remove the current 'Bar' and replace it with a new empty 'Bar', return the previous 'Bar'.
nextBar :: Notation value (Bar (ScoredNote value))
nextBar = do
  notes <- use notationNotes
  notationNoteCount .= 0
  notationNotes .= mempty
  case notes of
    []  -> return mempty
    [a] -> return a
    ax  -> return $ BarBranch $ Boxed.fromList ax

-- | Play a 'note' that will be tied to another 'note' at some point in the future. The tied note is
-- held for a time until the future 'untie'd note is reached.
tieNote
  :: ScorableNote note value
  => note -> Notation value (NoteReference, ScoredNote value)
tieNote = toNote >=> \ case
  n@ScoredNote{} -> do
    i <- notationTieID += 1 >> use notationTieID
    notationNotes %= (:) (BarLeaf n{ scoredNoteTiedID = NoteReference i })
    return (NoteReference i, n)
  ScoredNoteRest -> do
    return (untied, ScoredNoteRest)

-- | Shorthand for 'tieNote' returning only the 'NoteReference' and not the 'Note' constructed with
-- it.
tie :: ScorableNote note value => note -> Notation value NoteReference
tie = fmap fst . tieNote

-- | Stop playing a tied 'note'. Supply the 'NoteReference' returend by a previous call to 'tie',
-- pass the note value to which the note must be tied as @note1@ (passing 'RestNote' means to tie
-- the same note that initiated the 'tie'). The value @note2@ can be played with the note that is
-- being united at the time interval it is untied.
untie :: ScorableNote note value => NoteReference -> note -> Notation value ()
untie ref = toNote >=> \ case
  ScoredNoteRest -> return ()
  n@ScoredNote{} -> playNote n{ scoredNoteTiedID = ref }

----------------------------------------------------------------------------------------------------

newtype Composition a = Composition (StateT CompositionState (TFRandT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

data CompositionState
  = CompositionState
    { theCompositionMeasure  :: !Duration
    , theCompositionMoment   :: !Moment
    , theComposedInstruments :: Map.Map InstrumentID (NoteSequence PlayedTone)
    , theComposedDrums       :: NoteSequence PlayedDrum
    }

instance MonadState CompositionState Composition where
  state = Composition . state

instance MonadSplit TFGen Composition where
  getSplit = Composition $ lift getSplit

instance MonadRandom Composition where
  getRandomR  = Composition . lift . getRandomR
  getRandom   = Composition $ lift getRandom
  getRandomRs = Composition . lift . getRandomRs
  getRandoms  = Composition $ lift getRandoms

emptyComposition :: CompositionState
emptyComposition = CompositionState
  { theCompositionMeasure  = 0
  , theCompositionMoment   = 0
  , theComposedInstruments = Map.empty
  , theComposedDrums       = mempty
  }

getPlayedRoles :: CompositionState -> [PlayedRole PlayedTone]
getPlayedRoles comp = uncurry PlayedRole <$> Map.assocs (comp ^. composedInstruments)

-- | The amount of time for each measure. This parameter essentially sets the tempo of the music.
compositionMeasure :: Lens' CompositionState Duration
compositionMeasure = lens theCompositionMeasure $ \ a b -> a{ theCompositionMeasure = b }

compositionMoment :: Lens' CompositionState Moment
compositionMoment = lens theCompositionMoment $ \ a b -> a{ theCompositionMoment = b }

composedInstruments :: Lens' CompositionState (Map.Map InstrumentID (NoteSequence PlayedTone))
composedInstruments = lens theComposedInstruments $ \ a b -> a{ theComposedInstruments = b }

composedDrums :: Lens' CompositionState (NoteSequence PlayedDrum)
composedDrums = lens theComposedDrums $ \ a b -> a{ theComposedDrums = b }

runCompositionTFGen
  :: Composition a
  -> TFGen
  -> CompositionState
  -> IO ((a, CompositionState), TFGen)
runCompositionTFGen (Composition f) gen st = runTFRandT (runStateT f st) gen

-- | Evaluate a 'Notation' function to construct some playable notes for a given
-- 'ProcGen.Music.SoundFont.InstrumentID'.
composeNotes :: Notation notes a -> Composition a
composeNotes f = Composition $ lift $ TFRandT $ liftRandT $ liftIO . randGenNotation f

-- | Associate a 'Bar' defined by 'defineBar' with an 'InstrumentID', creating a 'NoteSequence' from
-- the given 'Bar' and setting the notest to time.
instrument :: InstrumentID -> Bar ScoredTone -> Composition ()
instrument inst bar = do
  t  <- use compositionMoment
  dt <- use compositionMeasure
  let seq = playNoteSequence t dt bar
  composedInstruments %= Map.alter (Just . maybe seq (<> seq)) inst

----------------------------------------------------------------------------------------------------

-- | This function produces a 'Bar' of a couple of played notes. Choose an
-- 'ProcGen.Music.SoundFont.InstrumentID', and then apply the exampe 'Bar' produced by this function
-- to the 'instrument' function. If you have no 'ProcGen.Music.SoundFont.InstrumentID' the sequencer
-- should default to a sine wave generator, so don't worry if you haven't created any instruments
-- yet, you can still use this example:
--
-- @
-- let randomSeed = 12345
-- let sinegen = InstrumentID "sine wave generator"
-- musicToFile "example.wav" randomSeed $ exampleComposition >>= instrument sinegen
-- @
exampleComposition :: Composition ()
exampleComposition = do
  let singen = InstrumentID "Sine Wave Generator"
  let a = intTone [39]
  let b = intTone [51]
  a_b_ <- composeNotes (do{ a; rest; b; rest; nextBar; })
  _ab_ <- composeNotes (do{ rest; a; b; rest; nextBar; })
  instrument singen (a_b_ <> _ab_)
