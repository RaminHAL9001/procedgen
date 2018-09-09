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
    Track(..), Target, Source,
    trackTime, trackSampleCount, newTrack, writeTrackFile, readTrackFile,
    -- * Sequencer Evaluation
    Sequencer, SequencerState(..), PlayToTrack(..),
    newSequencer, runSequencer, liftSynth, playRoleToTrack,
    addDrum, getDrum, addInstrument, addTone, getTone,
  ) where

import           ProcGen.Types
import           ProcGen.Arbitrary
import           ProcGen.Music.Composition
import           ProcGen.Music.KeyFreq88
import           ProcGen.Music.SoundFont
import           ProcGen.Music.Synth
import           ProcGen.Music.WaveFile

import           Control.Lens
import           Control.Monad.Random
import           Control.Monad.State

import qualified Data.Map                    as Map
import           Data.Semigroup
import qualified Data.Vector.Unboxed.Mutable as Unboxed

----------------------------------------------------------------------------------------------------

newtype Track = Track (Unboxed.IOVector Sample)

-- | When copying from one 'Track' to another, this function type denotes which 'Track' is the
-- target.
type Target a = a

-- | When copying from one 'Track' to another, this function type denotes which 'Track' is the
-- source.
type Source a = a

trackTime :: Track -> Duration
trackTime = sampleCountDuration . trackSampleCount

trackSampleCount :: Track -> SampleCount
trackSampleCount (Track vec) = Unboxed.length vec

newTrack :: MonadIO m => Duration -> m Track
newTrack = liftM Track . liftIO . Unboxed.new . durationSampleCount

writeTrackFile :: FilePath -> Track -> IO ()
writeTrackFile path (Track vec) = putRiffWaveFormatIO path vec

-- | Must be a @.WAV@ file, 44100 hz 16 bit signed little endian single channel.
readTrackFile :: FilePath -> IO Track
readTrackFile = fmap Track . getRiffWaveFormatIO

----------------------------------------------------------------------------------------------------

-- | This type class defines a 'playToTrack' function which can be instantiated by any data type
-- that can render a sound into a buffer. Sounds written by 'playToTrack' should overwrite whatever
-- exists in the buffer, no mixing of signals should occur in this step.
class PlayToTrack a where
  playToTrack :: Target Track -> Source a -> Sequencer ()

instance PlayToTrack Sound where
  playToTrack = error "TODO: ProcGen.Music.Sequencer.playToTrack :: Track -> Sound -> Sequencer ()"

instance PlayToTrack Track where
  playToTrack = error "TODO: ProcGen.Music.Sequencer.playToTrack :: Track -> Track -> Sequencer ()"

instance PlayToTrack TDSignal where
  playToTrack = error "TODO: ProcGen.Music.Sequencer.playToTrack :: Track -> TDSignal -> Sequencer ()"

playRoleToTrack :: Track -> Moment -> InstrumentID -> PlayedRole (Interval Note) -> Sequencer ()
playRoleToTrack = error "TODO: ProcGen.Music.Sequencer.playRoleToTrack"

----------------------------------------------------------------------------------------------------

newtype Sequencer a = Sequencer (StateT SequencerState IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState SequencerState Sequencer where { state = Sequencer . state; }

instance MonadRandom Sequencer where
  getRandomR  = liftTFRand . getRandomR
  getRandom   = liftTFRand getRandom
  getRandomRs = liftTFRand . getRandomRs
  getRandoms  = liftTFRand getRandoms

data SequencerState
  = SequencerState
    { theSequencerSynth        :: !SynthState
    , theSequencerDrumKit      :: !DrumKit
    , theSequencerInstruments  :: !(Map.Map InstrumentID ToneInstrument)
    }

sequencerSynth :: Lens' SequencerState SynthState
sequencerSynth = lens theSequencerSynth $ \ a b -> a{ theSequencerSynth = b }

sequencerDrumKit :: Lens' SequencerState DrumKit
sequencerDrumKit = lens theSequencerDrumKit $ \ a b -> a{ theSequencerDrumKit = b }

sequencerInstrument :: Lens' SequencerState (Map.Map InstrumentID ToneInstrument)
sequencerInstrument = lens theSequencerInstruments $ \ a b -> a{ theSequencerInstruments = b }

runSequencer :: Sequencer a -> SequencerState -> IO (a, SequencerState)
runSequencer (Sequencer f) = runStateT f

newSequencer :: IO SequencerState
newSequencer = do
  synth <- initSynth
  return SequencerState
    { theSequencerSynth       = synth
    , theSequencerDrumKit     = mempty
    , theSequencerInstruments = Map.empty
    }

-- | Evaluate a function of type 'ProcGen.Music.Synth.Synth' within a function of type
-- 'SequencerState'.
liftSynth :: Synth a -> Sequencer a
liftSynth f = do
  (a, synth) <- use sequencerSynth >>= liftIO . runSynth f
  sequencerSynth .= synth
  return a

-- | Evaluate a pure 'ProcGen.Arbitrary.TFRand' function within a 'ProcGen.Music.Synth.Synth'
-- function.
liftTFRand :: TFRand a -> Sequencer a
liftTFRand f = do
  (a, gen) <- runTFRand f <$> use (sequencerSynth . synthTFGen)
  sequencerSynth . synthTFGen .= gen
  return a

-- | Associate a 'DrumID' with a 'Sound', or append the 'Sound' to the 'SoundSet' if the 'DrumID'
-- already has one or more 'Sound's associated with it.
addDrum :: DrumID -> Sound -> Sequencer ()
addDrum drum sound = sequencerDrumKit %= addDrumToKit drum sound

-- | Select a sound for a given 'DrumID'. If more than one 'Sound' has been added to the same
-- 'DrumID', one of the 'Sound's will be selected at random.
getDrum :: DrumID -> Sequencer (Maybe Sound)
getDrum key = use (sequencerDrumKit . drumSounds key) >>= maybe (pure Nothing) chooseSound

-- | Create a new 'ToneInstrument' for use within this 'Sequencer', or update an existing
-- instrument.
addInstrument :: InstrumentID -> KeyIndex -> KeyIndex -> Sequencer InstrumentID
addInstrument instrm lo hi = do
  sequencerInstrument %= Map.insertWith (<>) instrm (toneInstrument lo hi)
  return instrm

addTone :: InstrumentID -> [ToneTag] -> ToneKeyIndicies -> Sound -> Sequencer ToneID
addTone instrm tags key sound = do
  let toneID    = ToneID key $ toneTagSet tags
  let newInstrm = uncurry toneInstrument $ minMaxKeyIndex key
  sequencerInstrument %=
    Map.alter (Just . addToneToInstrument toneID sound . maybe newInstrm id) instrm
  return toneID

getTone :: InstrumentID -> ToneID -> Sequencer (Maybe Sound)
getTone instrm toneID =
  (join . fmap (view $ toneSounds toneID) . Map.lookup instrm) <$> use sequencerInstrument >>=
  maybe (pure Nothing) chooseSound

