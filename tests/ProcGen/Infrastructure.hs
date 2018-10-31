module Main where


import           Prelude                     hiding (fail, length)

import           ProcGen.Arbitrary
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
import qualified Data.ByteString.UTF8        as UTF8Strict
import qualified Data.ByteString.Lazy        as BLazy
import           Data.Int
import qualified Data.Text                   as TStrict
import qualified Data.Vector                 as Boxed
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
      if t /= orig then throwError $ Decoded_value_is_wrong orig t else
        if BLazy.null rem then return t else
          throwError $ Unused_bytes t off $ BLazy.toStrict rem

test_VarWord35_protocol :: IO ()
test_VarWord35_protocol = do
  forM_ (zip [(0::Int) ..] p) $ \ (i, (orig, serialized, deserialized)) -> do
    when (mod i (1024*7) == (1024*7-1)) $ print (i+1)
    case deserialized of
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

----------------------------------------------------------------------------------------------------

data AnyPrim
  = Prim_UTFChar  UTFChar
  | Prim_Int      Int
  | Prim_Int8     Int8
  | Prim_Int16    Int16
  | Prim_Int32    Int32
  | Prim_Int64    Int64
  | Prim_Word     Word
  | Prim_Word8    Word8
  | Prim_Word16   Word16
  | Prim_Word32   Word32
  | Prim_Word64   Word64
  | Prim_Float    Float
  | Prim_Double   Double
  | Prim_String   BStrict.ByteString
  | Prim_VecInt8  (Unboxed.Vector Int8)
  | Prim_VecInt16 (Unboxed.Vector Int16)
  | Prim_VecInt32 (Unboxed.Vector Int32)
  deriving (Eq, Ord)

instance Show AnyPrim where
  showsPrec p = showParen (p > 10) . \ case
    Prim_UTFChar  a -> ("UTFChar "  ++) . showsPrec p a
    Prim_Int      a -> ("Int "      ++) . showsPrec p a
    Prim_Int8     a -> ("Int8 "     ++) . showsPrec p a
    Prim_Int16    a -> ("Int16 "    ++) . showsPrec p a
    Prim_Int32    a -> ("Int32 "    ++) . showsPrec p a
    Prim_Int64    a -> ("Int64 "    ++) . showsPrec p a
    Prim_Word     a -> ("Word "     ++) . showsPrec p a
    Prim_Word8    a -> ("Word8 "    ++) . showsPrec p a
    Prim_Word16   a -> ("Word16 "   ++) . showsPrec p a
    Prim_Word32   a -> ("Word32 "   ++) . showsPrec p a
    Prim_Word64   a -> ("Word64 "   ++) . showsPrec p a
    Prim_Float    a -> ("Float "    ++) . showsPrec p a
    Prim_Double   a -> ("Double "   ++) . showsPrec p a
    Prim_String   a -> ("Str0 "     ++) . showsPrec p a
    Prim_VecInt8  a -> ("Vec Int8 " ++) . showsPrec p a
    Prim_VecInt16 a -> ("Vec Int16 "++) . showsPrec p a
    Prim_VecInt32 a -> ("Vec Int32 "++) . showsPrec p a

primUTFChar :: Char -> AnyPrim
primUTFChar = Prim_UTFChar . UTFChar

primBounded :: forall a . (Eq a, Num a, Bounded a) => (a -> AnyPrim) -> [AnyPrim]
primBounded constr = (if (minBound :: a) == 0 then id else ((constr 0) :))
  [constr minBound, constr maxBound]

primValues :: Boxed.Vector (Boxed.Vector AnyPrim)
primValues = Boxed.fromList $ Boxed.fromList <$>
  [ primUTFChar <$> ['\0', 'A', '\x7F', '\x3BB']
  , primBounded Prim_Int
  , primBounded Prim_Int8
  , primBounded Prim_Int16
  , primBounded Prim_Int32
  , primBounded Prim_Int64
  , primBounded Prim_Word
  , primBounded Prim_Word8
  , primBounded Prim_Word16
  , primBounded Prim_Word32
  , primBounded Prim_Word64
  , [Prim_Float  0.0, Prim_Float  1.0e999, Prim_Float  1.0e-999]
  , [Prim_Double 0.0, Prim_Double 1.0e999, Prim_Double 1.0e-999]
  , Prim_String . UTF8Strict.fromString <$> ["", s2, s7_1, s7, s14_1, s14]
  , Prim_VecInt8 . Unboxed.fromList <$> [[], v2, v7_1, v7, v14_1, v14]
  ] where
    pow :: [n] -> Int -> [n]
    pow s i = mconcat $ replicate (2 ^ i) s
    s2 :: String
    s2    = "ABC "
    s7    = pow s2 5
    s7_1  = take (2^(7::Int) - 1 :: Int) s7
    s14   = pow s7 7
    s14_1 = take (2^(14::Int) - 1 :: Int) s14
    v2 :: (Unboxed.Unbox n, Num n) => [n]
    v2    = [37, 74, 111, 148]
    v7    = pow v2 5
    v7_1  = take (2^(7::Int) - 1 :: Int) v7
    v14   = pow v7 7
    v14_1 = take (2^(14::Int) - 1 :: Int) v14

writeAnyPrimToRow :: AnyPrim -> RowBuilder
writeAnyPrimToRow = \ case
  Prim_UTFChar  a -> writeRowPrim a
  Prim_Int      a -> writeRowPrim a
  Prim_Int8     a -> writeRowPrim a
  Prim_Int16    a -> writeRowPrim a
  Prim_Int32    a -> writeRowPrim a
  Prim_Int64    a -> writeRowPrim a
  Prim_Word     a -> writeRowPrim a
  Prim_Word8    a -> writeRowPrim a
  Prim_Word16   a -> writeRowPrim a
  Prim_Word32   a -> writeRowPrim a
  Prim_Word64   a -> writeRowPrim a
  Prim_Float    a -> writeRowPrim a
  Prim_Double   a -> writeRowPrim a
  Prim_String   a -> writeRowPrim a
  Prim_VecInt8  a -> writeRowPrim a
  Prim_VecInt16 a -> writeRowPrim a
  Prim_VecInt32 a -> writeRowPrim a

readAnyPrimMatching :: AnyPrim -> Select AnyPrim
readAnyPrimMatching prim = case prim of
  Prim_UTFChar  a -> read Prim_UTFChar  a
  Prim_Int      a -> read Prim_Int      a
  Prim_Int8     a -> read Prim_Int8     a
  Prim_Int16    a -> read Prim_Int16    a
  Prim_Int32    a -> read Prim_Int32    a
  Prim_Int64    a -> read Prim_Int64    a
  Prim_Word     a -> read Prim_Word     a
  Prim_Word8    a -> read Prim_Word8    a
  Prim_Word16   a -> read Prim_Word16   a
  Prim_Word32   a -> read Prim_Word32   a
  Prim_Word64   a -> read Prim_Word64   a
  Prim_Float    a -> read Prim_Float    a
  Prim_Double   a -> read Prim_Double   a
  Prim_String   a -> read Prim_String   a
  Prim_VecInt8  a -> read Prim_VecInt8  a
  Prim_VecInt16 a -> read Prim_VecInt16 a
  Prim_VecInt32 a -> read Prim_VecInt32 a
  where
    read :: (IsPrimitive a, Eq a) => (a -> AnyPrim) -> a -> Select AnyPrim
    read constr a = readRowPrim >>= \ b -> if a == b then return $ constr b else 
      throwCorruptRow $ TStrict.pack $
        "'Select' function failed\nexpected value: "++show prim++
        "\nselected value: "++show (constr b)

test_primitive_protocol :: IO ()
test_primitive_protocol = seedEvalTFRandT 0 $ forM_ [(0::Int) .. numTests] $ \ i -> do
  when (mod i 50 == 49) $ liftIO $ putStrLn $ "Completed "++show (i+1)++" tests..."
  let elem  = randSelect primValues -- each 'elem' is a set of values
  let unbox = Boxed.toList
  (a, b, c, d) <- (,,,) <$> elem <*> elem <*> elem <*> elem
  -- First run just tests of a single test set 'a'
  runTest $ unbox a
  -- Then run the cartesian product of four test sets
  mapM_ runTest $ (\ a b c d -> [a,b,c,d]) <$> unbox a <*> unbox b <*> unbox c <*> unbox d
  where
    numTests = 10000
    runTest elems = do
      let serialized   = execRowBuilder $ mapM_ writeAnyPrimToRow elems
      let env          = selectEnv serialized
      let deserialized = runRowSelect (mapM readAnyPrimMatching elems) env
      case deserialized of
        Left  err          -> liftIO $ do
          putStrLn $ "serialized object: " ++ show elems
          print err
          case err of
            CorruptRow{} -> do
              putStrLn $ hexDumpIfCorruptRow err
              print $ selectRowHeader $ corruptSelectEnv err
            _            -> print env
          fail "'Select' function failed."
        Right deserialized -> unless (elems == deserialized) $ liftIO $ do
          putStrLn $ "serialized object: " ++ show elems
          putStrLn $
            "    original: "++show elems++
            "\ndeserialized: "++show deserialized++
            "\n('Select' function succeeded but read elements do not match original elements)"
          fail "'Select' statement succeeded with incorrect result."

main :: IO ()
main = do
  putStrLn "Testing the VarWord35 bit serialization protocol..."
  test_VarWord35_protocol
  putStrLn "VarWor35 bit serialization tests passed.\n"
  --
  putStrLn "Testing the primitive value serialization protocol..."
  test_primitive_protocol
  putStrLn "Primitive value serialization protocol tests passed."
