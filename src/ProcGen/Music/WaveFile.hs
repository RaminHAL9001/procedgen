-- | This module provide 'Data.Binary.Binary' methods for generating a @RIFF` formatted @.WAV@ file.
--
-- Currently only supports 44.1K/sec rate, 16-bit signed little-endian formatted samples.
module ProcGen.Music.WaveFile
  ( sizeOfRiffHeader, putRiffWaveFormat, getRiffWaveFormat,
    readWave, writeWave, hReadWave, hWriteWave,
  )
  where

import           ProcGen.Types

import           Control.Monad
import           Control.Monad.ST

import qualified Data.Binary                 as Binary
import qualified Data.Binary.Get             as Binary
import qualified Data.Binary.Put             as Binary
import qualified Data.ByteString.Lazy        as Bytes
import           Data.Char
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable
import           Data.Word

import           System.IO (Handle)

----------------------------------------------------------------------------------------------------

sizeOfRiffHeader :: Int
sizeOfRiffHeader = 44

rhsiz :: Word32
rhsiz = fromIntegral sizeOfRiffHeader

-- sample size in bytes
sz1 :: Word16
sz1 = 2

sz2 :: Word32
sz2 = fromIntegral sz1 -- sample size in bytes

putMagic :: [Char] -> Binary.Put
putMagic = mapM_ $ Binary.putWord8 . fromIntegral . ord

putRiffWaveFormat :: Unboxed.Vector Sample -> Binary.Put
putRiffWaveFormat vec = do
  let siz = Unboxed.length vec
  let sr = round sampleRate
  let sz2 = fromIntegral sz1 :: Word32 -- sample size in bytes
  putMagic    "RIFF"  -- file magic number (4)
  -- the whole file size including the 44-byte header must be stored next
  Binary.putWord32le $ rhsiz + sz2 * fromIntegral siz -- (4)
  putMagic    "WAVE"  -- begin content section (4)
  putMagic    "fmt "  -- begin format section  (4)
  Binary.putWord32le    16    -- length of "fmt " section (4)
  Binary.putWord16le     1    -- data type ID, 1 indicates pulse-code modulated, 3 indicates IEEE754 (2)
  Binary.putWord16le     1    -- number of channels (2)
  Binary.putWord32le  sr      -- samples per second (4)
  Binary.putWord32le (sr*sz2) -- data rate in bytes per second (4)
  Binary.putWord16le     sz1  -- bytes per sample (2)
  Binary.putWord16le    16    -- bits per sample (2)
  putMagic           "data"   -- begin data section (4)
  Binary.putWord32le $ sz2 * fromIntegral siz -- data size (4)
  forM_ (Unboxed.toList vec) $ Binary.putWord16le . fromIntegral . toPulseCode -- data

getMagic :: String -> [Char] -> Binary.Get ()
getMagic err cx = case cx of
  []   -> return ()
  c:cx -> Binary.getWord8 >>= \w -> if fromEnum c == fromEnum w then getMagic err cx else fail err

packRiffWave
  :: Int -> Int
  -> (forall s . ST s (Mutable.STVector s Sample))
  -> Binary.Get (Unboxed.Vector Sample)
packRiffWave isiz i next = seq next $! if i >= isiz then return (Unboxed.create next) else do
  samp <- toSample . fromIntegral <$> Binary.getWord16le
  packRiffWave isiz (i + 1) (next >>= \ vec -> Mutable.write vec i samp >> return vec)

getRiffWaveFormat :: Binary.Get (Unboxed.Vector Sample)
getRiffWaveFormat = do
  let hdr err get expct = get >>= \i -> if i==expct then return () else fail err
  let sr = round sampleRate
  getMagic "does not appear to be a \".wav\" file"                        "RIFF"
  filesiz <- Binary.getWord32le
  getMagic "unexpected file content header"                               "WAVE"
  getMagic "unexpected file format header"                                "fmt "
  hdr "invalid format section header size"                                Binary.getWord32le    16
  hdr "cannot load the type of data in this file"                         Binary.getWord16le     1
  hdr "requires mono sound, this file contains stereo sound"              Binary.getWord16le     1
  hdr "can only use PCM data sampled at 44100 per second"                 Binary.getWord32le  sr
  hdr "can only use PCM data rated at 88200 bytes per second throughput"  Binary.getWord32le (sr*sz2)
  hdr "can only use PCM data samples of 2-bytes per sample"               Binary.getWord16le     sz1
  hdr "requires 16-bit signed integer little-endian encoded PCM data"     Binary.getWord16le    16
  getMagic "\".wav\" file contains valid header but no data section"      "data"
  siz <- Binary.getWord32le
  let isiz = fromIntegral $ div siz sz2
  if siz<0 then fail "data size is a negative value" else
    if fromIntegral (siz+rhsiz) /= filesiz then fail "incorrect file header size" else
      if siz==0 then return Unboxed.empty else packRiffWave isiz 0 $ Mutable.new isiz

-- | Read an entire RIFF/WAVE file from an already-opened file 'System.IO.Handle'. The
-- 'System.IO.Handle' is not closed when completed.
hReadWave :: Handle -> IO (Unboxed.Vector Sample)
hReadWave h = Binary.runGet getRiffWaveFormat <$> Bytes.hGetContents h

-- | Write an entire RIFF/WAVE file into an already-opened file 'System.IO.Handle'. The
-- 'System.IO.Handle' is not closed when completed.
hWriteWave :: Handle -> Unboxed.Vector Sample -> IO ()
hWriteWave h = Bytes.hPut h . Binary.runPut . putRiffWaveFormat

-- | Open a given 'System.IO.FilePath' and read the entire contents as RIFF/WAVE formatted binary
-- data, returning a 'Unboxed.Vector' of 'ProcGen.Types.Sample' data contained within.
readWave :: FilePath -> IO (Unboxed.Vector Sample)
readWave = fmap (Binary.runGet getRiffWaveFormat) . Bytes.readFile

-- | Open a given 'System.IO.FilePath' and write an entire 'Unboxed.Vector' of
-- 'ProcGen.Types.Samples's into it as RIFF/WAVE formatted binary data.
writeWave :: FilePath -> Unboxed.Vector Sample -> IO ()
writeWave path = Bytes.writeFile path . Binary.runPut . putRiffWaveFormat
