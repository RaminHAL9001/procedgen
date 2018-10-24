module Main where


import           Prelude                     hiding (fail, length)

import           ProcGen.TinyRelDB
import           ProcGen.PrimeNumbers

import           Control.Arrow
import           Control.Applicative
import           Control.Monad.Except        hiding (fail)
import           Control.Monad.Fail

import qualified Data.Binary                 as Bin
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString             as BStrict
import qualified Data.ByteString.Lazy        as BLazy
import           Data.Int
import qualified Data.Text                   as TStrict
import qualified Data.Vector.Unboxed         as Unboxed
import           Data.Word

import           Numeric

----------------------------------------------------------------------------------------------------

data TEST a = T1 a | T2 a a | T3 a a a | T4 a a a a
  deriving (Eq, Ord, Show)

data TESTError
  = Decoder_failed
    { remaining_bytes     :: !BStrict.ByteString
    , consumed_byte_count :: !ByteOffset
    , fail_message        :: !TStrict.Text
    }
  | Decoded_value_is_wrong
    { decoded_value       :: !(TEST Int64)
    , expected_value      :: !(TEST Int64)
    }
  | Unused_bytes
    { decoded_value       :: !(TEST Int64)
    , consumed_byte_count :: !ByteOffset
    , remaining_bytes     :: !BStrict.ByteString
    }
  deriving Show

newtype HexWord8 = HexWord8 Word8 deriving (Eq, Ord)
instance Show HexWord8 where
  show (HexWord8 w) = '0' : 'x' : (if w < 0x10 then ('0' :) else id) (showHex w "")

showBase2 :: FiniteBits w => w -> String
showBase2 w = let bz = finiteBitSize w - 1 in
  [if testBit w i then '*' else '-' | i <- [bz, bz - 1 .. 0]]

showPutBase2 :: Put -> String
showPutBase2 = BLazy.unpack . runPut >=> showBase2

regularSpacesR :: Int -> String -> String
regularSpacesR n = reverse . regularSpacesL n . reverse

regularSpacesL :: Int -> String -> String
regularSpacesL n = loop where
  loop  str = case splitAt n str of
    ("" , ""  ) -> ""
    (str, more) -> str ++ ' ' : loop more

newtype RunTest a
  = RunTest{ unwrapRunTest :: ExceptT TESTError Get a }
  deriving (Functor, Applicative, Monad)

instance Alternative RunTest where { (<|>) = mplus; empty = mzero; }

instance MonadPlus RunTest where
  mzero = RunTest $ ExceptT mzero
  mplus (RunTest (ExceptT a)) (RunTest (ExceptT b)) = RunTest $ ExceptT $ mplus a b

instance MonadError TESTError RunTest where
  throwError = RunTest . throwError
  catchError (RunTest try) catch = RunTest $ catchError try $ unwrapRunTest . catch

showBytes :: BStrict.ByteString -> String
showBytes = show . fmap HexWord8 . BStrict.unpack

liftBinGet :: Get a -> RunTest a
liftBinGet get = RunTest $ ExceptT $ Right <$> get

execTest
  :: BLazy.ByteString -> RunTest a
  -> Either TESTError (BLazy.ByteString, ByteOffset, a)
execTest bytes (RunTest f) = case test of
  Left (bytes, off, err) -> Left $
    Decoder_failed (BLazy.toStrict bytes) off (TStrict.pack err)
  Right (bytes, off, a) -> case a of
    Left err -> Left err
    Right  a -> Right (bytes, off, a)
  where
    test = flip runGetOrFail bytes $ runExceptT f

putTEST :: TEST Int64 -> BStrict.ByteString
putTEST = BLazy.toStrict . runPut . \ case
  T1 a       -> p a
  T2 a b     -> p a >> p b
  T3 a b c   -> p a >> p b >> p c
  T4 a b c d -> p a >> p b >> p c >> p d
  where
    p = binPutVarWord35 . varWord35

getTEST
  :: TEST Int64
  -> BLazy.ByteString
  -> Either TESTError
            (BLazy.ByteString, ByteOffset, TEST Int64)
getTEST orig bytes = execTest bytes $ unVarWord35 <$> liftBinGet Bin.get >>= \ a ->
  check (T1 a)     $ unVarWord35 >>> \ b -> 
  check (T2 a b)   $ unVarWord35 >>> \ c -> 
  check (T3 a b c) $ unVarWord35 >>> \ d -> done (T4 a b c d)
  where
    check t next = mplus (liftBinGet Bin.get >>= next) (done t)
    done  t = do
      rem <- liftBinGet getRemainingLazyByteString
      off <- liftBinGet bytesRead
      if t == orig
       then if BLazy.null rem
             then return t
             else throwError $ Unused_bytes t off $ BLazy.toStrict rem
       else throwError $ Decoded_value_is_wrong orig t

main :: IO ()
main = forM_ (loop (0::Int) p) $ \ (n, group) -> do
  print n
  putStrLn $ "max bound: " ++ show (maximum $ p1 ++ p2 ++ p3)
  forM_ group $ \ (orig, serialized, deserialized) -> case deserialized of
    Right{}  -> return ()
    Left err -> do
      case err of
        Decoder_failed remainder offset message -> do
          putStrLn $ "ERROR: " ++ TStrict.unpack message
          putStrLn $ "     test: " ++ show (orig :: TEST Int64)
          putStrLn $ " original: " ++ showBytes serialized
          putStrLn $ "remainder: " ++ showBytes remainder
          putStrLn $ "           " ++ replicate (1 + 5 * fromIntegral offset) ' ' ++ "^"
          putStrLn $ "   offset: " ++ show offset
        Decoded_value_is_wrong original decoded -> do
          putStrLn $ "ERROR: decoder succeeded with wrong result"
          putStrLn $ "original: " ++ show original
          putStrLn $ " decoded: " ++ show decoded
        Unused_bytes decoded offset bytes -> do
          putStrLn $ "ERROR: decoded correctly, but left undecoded bytes"
          putStrLn $ " decoded: " ++ show decoded
          putStrLn $ "  offset: " ++ show offset
          putStrLn $ "   extra: " ++ showBytes bytes
      fail "Tests failed."
  where
    lim = flip mod (varWord35Mask + 1)
    p1  = 0 : 1 : (fromIntegral <$> Unboxed.toList all16BitPrimes)
    p2  = (\a -> lim $  a * a  ) <$> p1
    p3  = (\a -> lim $ a * a * a) <$> p1
    p   = do
      (a, b, c) <- zip3 p1 p2 p3
      orig <- [T1 c, T2 a b, T3 a b c, T4 0 a b c, T4 a 0 b c, T4 a b 0 c, T4 a b c 0]
      let serialized = putTEST orig
      [(orig, serialized, getTEST orig $ BLazy.fromStrict serialized)]
    loop n p = seq n $! case splitAt (7*1024) p of
      ([],   []) -> []
      (group, p) -> (n, group) : loop (n + 7*1024) p
