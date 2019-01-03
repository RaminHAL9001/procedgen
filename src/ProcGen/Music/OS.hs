{-# LANGUAGE CPP #-}

-- Short-circuit the linux drivers for now, rather than depending on a parameter set by Cabal.
#define GNU_LINUX 1

-- | Operating system specific API for the pulse code modulator (PCM) device.
module ProcGen.Music.OS
  ( AudioDevice(..), defaultDevice,
    SignalCallback(..), testSignalCallback,
    AudioPlaybackThread, startPlayback, startPlaybackTo, stopPlayback,
    Data.Int.Int16,
    module Control.Monad.State,
    module Control.Monad.Reader,
  ) where

import           ProcGen.Types

import           Control.Concurrent
import           Control.Monad.State
import           Control.Monad.Reader

import           Data.Int
import qualified Data.Text as Strict

import           Foreign.Storable

#if GNU_LINUX
import qualified Sound.ALSA.PCM as Linux
#endif

-- | A signal callback should take a function that returns a right and left channel, even on
-- mono-channel devices. Every time this function is called back, it must return the next signal
-- value for both channels as a 32-bit signed integer. Mono (1-channel) devices will only take the
-- 'Prelude.fst' channel.
data SignalCallback st
  = SignalCallback
    { signalStartAudioFrame    :: ReaderT AudioDevice (StateT st IO) Bool
      -- ^ This function is called before each audio frame begins copying to the operating system
      -- buffer. Return 'Prelude.False' to report to the OS that the audio session has completed and
      -- no more audio should be output.
    , signalProduceNextSamples :: Int -> ReaderT AudioDevice (StateT st IO) (Int16, Int16)
      -- ^ This function should produce two samples to be copied out to the PCM buffer, left and
      -- right audio channels. This function takes an integer indicating the index of the current
      -- buffer frame to which the samples will be copied.
    }

data AudioDevice
  = AudioDevice
    { audioDeviceID         :: !Strict.Text
      -- ^ A string used to tell the operating system which output channel to use. The string
      -- formatting is operating-system specific. A default playback device should be provided by
      -- the 'defaultDevice' function.
    , audioDeviceSampleRate :: !Frequency
      -- ^ Sample rate, usually 22050, 44100, or 48000.
    , audioDeviceStereo     :: !Bool
      -- ^ True if the output is stereo (2-channel), false if mono (1-channel).
    }
  deriving (Eq, Ord, Show, Read)

newtype AudioPlaybackThread = AudioPlayback ThreadId
  deriving (Eq, Show)

-- | This can be used to test the 'startPlayback' function, it will generate a sinusoidal signal at
-- 256 Hz alternating back and forth between the left and right speakers every second.
--
-- @
-- testAudioThread <- startPlayback (return 0) testSignalCallback
-- -- Check if you can hear anything in both speakers...
-- stopPlayback testAudioThread
-- @
testSignalCallback :: SignalCallback Int
testSignalCallback = SignalCallback
  { signalStartAudioFrame    = return True
  , signalProduceNextSamples = const $ do
      t <- state $ \ i -> (2 * pi * realToFrac i / sampleRate :: Moment, i+1)
      let vol  = 1 + sin t / 2
      let samp = sin (256.0*t)
      return (toPulseCode $ samp * vol, toPulseCode $ samp * (1 - vol))
  }

stopPlayback :: AudioPlaybackThread -> IO ()
stopPlayback (AudioPlayback thread) = killThread thread

-- | Calls 'startPlaybackTo' on the 'defaultDevice', returns an audio-playback thread. Playback
-- continues until 'stopPlayback' is called.
startPlayback :: IO st -> SignalCallback st -> IO AudioPlaybackThread
startPlayback = startPlaybackTo defaultDevice

----------------------------------------------------------------------------------------------------
#if GNU_LINUX

newtype LinuxHandle st y = LinuxHandle (MVar st)

soundSource
  :: forall st . AudioDevice -> IO st -> SignalCallback st
  -> Linux.SoundSource (LinuxHandle st) Int16
soundSource dev newSt callback = Linux.SoundSource
  { Linux.soundSourceOpen  = LinuxHandle <$> (newSt >>= newMVar) --newEmptyMVar
  , Linux.soundSourceClose = \ (LinuxHandle _mvar) -> return () --void $ takeMVar mvar
  , Linux.soundSourceStart = \ (LinuxHandle _mvar) -> return () --newSt >>= putMVar mvar
  , Linux.soundSourceStop  = \ (LinuxHandle _mvar) -> return () --void $ takeMVar mvar
  , Linux.soundSourceRead  = \ (LinuxHandle mvar) ptr siz -> modifyMVar mvar $ \ st -> do
      (st, continue) <- run dev st $ signalStartAudioFrame callback
      if not continue then return (st, 0) else
        let loop i = if i >= siz then return i else do
              let sz = sizeOf (1::Int16)
              (sampL, sampR) <- signalProduceNextSamples callback $! div i 2
              liftIO $ pokeElemOff ptr (i + 0 * sz) sampL
              liftIO $ pokeElemOff ptr (i + 1 * sz) sampR
              loop $! i + 2 * sz
        in  run dev st $ loop 0
  }

-- not for export
run :: AudioDevice -> st -> ReaderT AudioDevice (StateT st IO) a -> IO (st, a)
run dev st f = runStateT (runReaderT f dev) st >>= \ (a, st) -> return (st, a)

defaultDevice :: AudioDevice
defaultDevice = AudioDevice
  { audioDeviceID         = "pulse"
  , audioDeviceSampleRate = sampleRate
  , audioDeviceStereo     = True
  }

-- | Starts a thread which continuously outputs a signal to the operating system's audio PCM device,
-- returning a handle to the audio thread of type 'AudioPlaybackThread'. Playback continues until
-- 'stopPlayback' is called on the handle. The 'SignalCallback' must continue to produce samples to
-- send to the PCM for as long as the thread is alive, so even if there is nothing to play, zero
-- values should be returned.
startPlaybackTo :: AudioDevice -> IO st -> SignalCallback st -> IO AudioPlaybackThread
startPlaybackTo dev newSt callback = fmap AudioPlayback $ forkOS $ do
  let channels  = if audioDeviceStereo dev then 2 else 1
  let numFrames = round $ audioDeviceSampleRate dev / animationRate
  let sink = Linux.alsaSoundSink
        (Strict.unpack $ audioDeviceID dev)
        ((Linux.SoundFmt $ round $ audioDeviceSampleRate dev) :: Linux.SoundFmt Int16)
  Linux.copySound (soundSource dev newSt callback) sink (channels * numFrames)

#endif
-- GNU_LINUX
----------------------------------------------------------------------------------------------------
