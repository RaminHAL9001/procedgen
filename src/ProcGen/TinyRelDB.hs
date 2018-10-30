-- | A small relational database engine built-in to ProcGen so there is no need for a dependency on
-- Sqlite or any Haskell relational database mappers. The database is not entirely relational, and
-- really only provides functionality similar to a simplified spreadsheet program that operates on
-- binary CSV files, or on no files at all, everything can be done in memory only. This turns out to
-- be a useful abstraction for pretty printers and text editors.
module ProcGen.TinyRelDB
  ( -- * Database Data Types
    PlainRow, TaggedRow, Table, taggedRowBytes, tableToSequence, hexDumpTaggedRow,
    -- * Mapping Haskell Data Types
    DBMapped(..), Select, SelectAnomaly(..), SelectEnv(..), RowBuilder, RowBuilderM,
    selectEnv, runRowSelect, selectedElemCount, execRowBuilder, runRowBuilder,
    writtenElemCount,
    putPrimLazyUTF8String, getPrimLazyUTF8String,
    putPrimStrictUTF8String, getPrimStrictUTF8String,
    putPrimUnboxedVector, getPrimUnboxedVector,
    writeRowPrim, tableFoldDown, tableFoldUp,
    nextRowTag, skipToRowElem, throwUnlessRowEnd, rowElem, unpackRowElem, readRowPrim,
    currentTaggedRow, throwCorruptRow, hexDumpIfCorruptRow,
    -- * Classes of Primitives
    IsNumericPrimitive(..), IsPrimitive(..),
    putNumericPrimitive,
    UTFChar(..),
    -- * Low-Level Row Formatting
    RowTagBytes, rowElemBytes,
    RowHeaderElem(..), RowElemOffset, RowElemLength, RowHeaderTable,
    RowHeader, rowHeader, indexHeaderTag,
    RowTag(..), indexElemTypeSize,
    PrimElemType(..), primElemTypeSize,
    aggregA, aggreg,
    -- * 35-Bit Variable-Width Word
    VarWord35, varWord35, unVarWord35, varWord35Mask, binGetVarWord35, binPutVarWord35,
    -- * Packed Vectors
    toByteString, (!?), (!), length, fromList, toList,
  ) where

import           Prelude                     hiding (fail, length)

import           Control.Arrow
import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except        hiding (fail)
import           Control.Monad.Fail
import           Control.Monad.Reader        hiding (fail)
import           Control.Monad.State         hiding (fail)

import qualified Data.Binary                 as Bin
import           Data.Binary.Builder            (Builder)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString             as BStrict
import qualified Data.ByteString.UTF8        as UTF8Strict
import qualified Data.ByteString.Lazy        as BLazy
import qualified Data.ByteString.Lazy.UTF8   as UTF8Lazy
import           Data.Char
import           Data.Int
import           Data.Proxy
import qualified Data.Sequence               as S
import           Data.Semigroup
import qualified Data.Text                   as TStrict
import qualified Data.Text.Lazy              as TLazy
import           Data.Vector.Unboxed.Base    (Unbox)
import qualified Data.Vector.Unboxed         as Unboxed
import           Data.Word

import           Text.Printf                 (printf)

import           Numeric                     (showHex)

----------------------------------------------------------------------------------------------------

-- | Create an aggregation function from an applicative fold function. The pure fold function will
-- be used to zip two rows. The first @row@ parameter is a default @row@ value to pass when one row
-- is longer than the other.
aggregA
  :: Applicative f
  => row                    -- ^ the element to place when one row is longer than another
  -> (row -> row -> f fold) -- ^ the element-wise sumation function
  -> [row] -> [row] -> f [fold]
aggregA def add ax bx = loop (ax, bx) where
  loop = \ case
    ([]  , []  ) -> pure []
    ([]  , b:bx) -> (:) <$> add def b   <*> loop ([], bx)
    (a:ax, []  ) -> (:) <$> add a   def <*> loop (ax, [])
    (a:ax, b:bx) -> (:) <$> add a   b   <*> loop (ax, bx)

-- | A pure version of 'aggregA'.
aggreg :: row -> (row -> row -> fold) -> [row] -> [row] -> [fold]
aggreg def add ax bx = runIdentity $ aggregA def (\ a -> pure . add a) ax bx

----------------------------------------------------------------------------------------------------

-- | A wrapper around an ordinary 'Prelude.Char' type that instantiates 'Bin.Binary' such that when
-- it is serialized, it always consumes exactly 3-bytes. 'UTF8Char' also instantiates 'IsPrimitive'
-- and 'PackedVectorElem'.
newtype UTFChar = UTFChar{ unUTFChar :: Char }
  deriving (Eq, Ord)

instance Show UTFChar where
  showsPrec p (UTFChar c) = showsPrec p c
  showList = showList . fmap unUTFChar

instance Bin.Binary UTFChar where
  put = putUTFChar
  get = getUTFChar

putUTFChar :: UTFChar -> Put
putUTFChar (UTFChar char) = do
  let c = ord char
  let w = fromIntegral . (.&. 0xFF) :: Int -> Word8
  b <- pure $ shift c 8
  a <- pure $ shift b 8
  putWord8 (w a) >> putWord8 (w b) >> putWord8 (w c)

getUTFChar :: Get UTFChar
getUTFChar = do
  let w i = (`shift` i) . fromIntegral :: Word8 -> Int
  isolate 3 $ (\ a b c -> UTFChar $ chr $ w 16 a * w 8 b * w 0 c) <$>
    getWord8 <*> getWord8 <*> getWord8

----------------------------------------------------------------------------------------------------

-- | A variable-length unsigned integer used to record binary blob sizes in element indicies. A
-- 'VarWord35' consists of up to @5*7 = 35@ bits, and since @2^35@ is 32 Gibibytes, this will cover
-- all practical blob sizes for the forseeable future. The serialized form of this number is
-- big-endian 'Data.Word.Word8' values with the highest significant bit of each 'Data.Word.Word8'
-- octat set to 1 if it is not the final value in the number, and set to 0 if it is the final value
-- of the number. The remaining lower 7 bits (the septet) are the content of the value. There are up
-- to 5 septets, however many are necessary to store the full numerical value. Constructing a
-- 'BStrict.ByteString' with more than 5 octets with the highest significant bit set and parsing a
-- 'VarWord35 will evaluate to an error, regardless of the content of the lower septet bits of the
-- octet.
newtype VarWord35 = VarWord35{ unVarWord35 :: Int64 }
  deriving (Eq, Ord)

instance Show VarWord35 where
  showsPrec p (VarWord35 a) = showParen (p > 10) $ ("varWord35 " ++) . showsPrec p a

instance Bounded VarWord35 where
  minBound = VarWord35 0
  maxBound = VarWord35 varWord35Mask

instance Bin.Binary VarWord35 where
  put = binPutVarWord35
  get = binGetVarWord35

-- | Construct a 'VarWord35'. Takes any 'Prelude.Integral' data type modulo the 'varWord35Mask'.
varWord35 :: Integral i => i -> VarWord35
varWord35 = VarWord35 . (.&. varWord35Mask) . fromIntegral

-- | This bit mask (expressed as a 64-bit integer) is bitwise-ANDed with an arbitrary
-- 'Data.Int.Int64' value to construct a 'VarWord35'.
varWord35Mask :: Int64
varWord35Mask = 0x00000007FFFFFFFF

-- | This function is used to instantiate 'Bin.put' in the 'Bin.Binary' typeclass. It is exported
-- here under a more specific name and type.
binPutVarWord35 :: VarWord35 -> Bin.Put
binPutVarWord35 (VarWord35 w64) =
  if w64 <= varWord35Mask
   then loop 0 w64 (pure ())
   else error ("binPutVarWord35 "++showHex w64 "")
  where
    loop mask w64 = let w8 = fromIntegral $ w64 .&. 0x7F in
      if w64 < 0x80 then ((putWord8 $! w8 .|. mask) >>) else
        (loop 0x80 $! shift w64 (-7)) . ((putWord8 $! mask .|. w8) >>)

-- | This function is used to instantiate 'Bin.get' in the 'Bin.Binary' typeclass. It is exported
-- here under a more specific name and type.
binGetVarWord35 :: Bin.Get VarWord35
binGetVarWord35 = VarWord35 <$> loop (0 :: Int) 0 where
  loop i accum = if i > 5
   then error "decoded VarWord35 byte string with more than 5 x 7-bit bytes"
   else Bin.getWord8 >>= \ w8 -> (if w8 .&. 0x80 == 0 then pure else loop $! i + 1) $!
          fromIntegral (w8 .&. 0x7F) .|. shift accum 7

----------------------------------------------------------------------------------------------------

-- | 'RowTag' types 'UTF8String' and 'BinaryBlob' do not take a 'FormElemClass' and so 6 bits of
-- the 'Data.Word.Word8' are used to record the serialized index value's blob size are prepended as
-- higher significant digits to the 'VarWord35', with the 6th bit indicating whether there is a
-- 'VarWord35' following in the serialized blob, and the lower 5 bits used as a multiplier to the
-- 'VarWord35'. So in actual fact, the 'UTF8String' and 'BinaryBlob' types can store blobs of
-- @2^(5+35) == 2^40@ bytes, which is a Tebibyte. This exceeds the memory capacity of most modern
-- computer systems.
newtype VarWord40 = VarWord40 Int64
  deriving (Eq, Ord, Show)

instance Bounded VarWord40 where
  minBound = VarWord40 0
  maxBound = VarWord40 0x000000FFFFFFFFFF

varWord40 :: Word8 -> VarWord35 -> VarWord40
varWord40 w8 (VarWord35 w64) = VarWord40 $ shift (fromIntegral $ w8 .&. 0x1F) 35 .|. w64

splitVarWord40 :: VarWord40 -> (Word8, VarWord35)
splitVarWord40 (VarWord40 w64) = let (VarWord35 mask) = maxBound in
  (fromIntegral $ shift (w64 .&. 0x000000F800000000) (-35), VarWord35 $ w64 .&. mask)

----------------------------------------------------------------------------------------------------

data RowTag
  = NumericPrimitive !PrimElemType
    -- ^ numerical primitive
  | HomoArray        !PrimElemType !VarWord35
    -- ^ Arrays of homogeneous elements. Can be used to store strings.
  | UTF8String       !VarWord40
    -- ^ A string with non-uniform character elements.
  | BinaryBlob       !VarWord40
    -- ^ Any 'BStrict.ByteString'
  deriving (Eq, Ord)

instance Show RowTag where
  showsPrec p = \ case
    NumericPrimitive t -> showsPrec p t
    HomoArray      t n -> showsPrec p t . ('[' :) . showsPrec p n . (']' :)
    UTF8String       n -> ("String[" ++) . showsPrec p n . (']' :)
    BinaryBlob       n -> ("Blob[" ++) . showsPrec p n . (']' :)

instance Bin.Binary RowTag where
  put = putIndexElem
  get = getIndexElem

indexElemBinPrefix :: RowTag -> Word8
indexElemBinPrefix = (`shift` 6) . \ case
  NumericPrimitive{} -> 0
  HomoArray{}        -> 1
  UTF8String{}       -> 2
  BinaryBlob{}       -> 3

indexElemTypeSize :: RowTag -> Int
indexElemTypeSize = \ case
  NumericPrimitive       e  -> primElemTypeSize e
  HomoArray e (VarWord35 x) -> checkSize $ fromIntegral (primElemTypeSize e) * x
  UTF8String  (VarWord40 w) -> checkSize w
  BinaryBlob  (VarWord40 w) -> checkSize w
  where
    max = maxBound :: Int
    checkSize :: Int64 -> Int
    checkSize siz =  if siz < fromIntegral max then fromIntegral siz else error $
      "'TaggedRow' header contains 'RowTag' which points to an object of size "++show siz++
      " bytes, but the maximum size of a 'TaggedRow' is "++show max++" bytes"

----------------------------------------------------------------------------------------------------

data PrimElemType
  = PrimTypeHostInt
  | PrimTypeHostWord
  | PrimTypeUTFChar
  | PrimTypeInt8
  | PrimTypeWord8
  | PrimTypeInt16
  | PrimTypeWord16
  | PrimTypeInt32
  | PrimTypeWord32
  | PrimTypeInt64
  | PrimTypeWord64
  | PrimTypeFloat
  | PrimTypeDouble
  deriving (Eq, Ord, Enum, Bounded)

instance Show PrimElemType where
  show = \ case
    PrimTypeHostInt   -> "HostInt"
    PrimTypeHostWord  -> "HostWord"
    PrimTypeUTFChar   -> "UTFChar"
    PrimTypeInt8      -> "Int8"
    PrimTypeWord8     -> "Word8"
    PrimTypeInt16     -> "Int16"
    PrimTypeWord16    -> "Word16"
    PrimTypeInt32     -> "Int32"
    PrimTypeWord32    -> "Word32"
    PrimTypeInt64     -> "Int64"
    PrimTypeWord64    -> "Word64"
    PrimTypeFloat     -> "Float"
    PrimTypeDouble    -> "Double"

primElemTypeBinSuffix :: PrimElemType -> Word8
primElemTypeBinSuffix = fromIntegral . fromEnum

-- | The serialized size (in bytes) of a value represented by the 'PrimElemType'.
primElemTypeSize :: PrimElemType -> Int
primElemTypeSize = \ case
  PrimTypeUTFChar  -> primTypeUTFCharSize
  PrimTypeHostInt  -> numericPrimitiveSize (Proxy :: Proxy Int)
  PrimTypeInt8     -> numericPrimitiveSize (Proxy :: Proxy Int8)
  PrimTypeInt16    -> numericPrimitiveSize (Proxy :: Proxy Int16)
  PrimTypeInt32    -> numericPrimitiveSize (Proxy :: Proxy Int32)
  PrimTypeInt64    -> numericPrimitiveSize (Proxy :: Proxy Int64)
  PrimTypeHostWord -> numericPrimitiveSize (Proxy :: Proxy Word)
  PrimTypeWord8    -> numericPrimitiveSize (Proxy :: Proxy Word8)
  PrimTypeWord16   -> numericPrimitiveSize (Proxy :: Proxy Word16)
  PrimTypeWord32   -> numericPrimitiveSize (Proxy :: Proxy Word32)
  PrimTypeWord64   -> numericPrimitiveSize (Proxy :: Proxy Word64)
  PrimTypeFloat    -> numericPrimitiveSize (Proxy :: Proxy Float)
  PrimTypeDouble   -> numericPrimitiveSize (Proxy :: Proxy Double)

putIndexElem :: RowTag -> Put
putIndexElem e = case e of
  NumericPrimitive p     -> putWord8 $ pfx .|. primElemTypeBinSuffix p
  HomoArray        p w35 -> putWord8 (pfx .|. primElemTypeBinSuffix p) >> Bin.put w35
  UTF8String         w40 -> putWord40 w40
  BinaryBlob         w40 -> putWord40 w40
  where
    pfx = indexElemBinPrefix e
    putWord40 w40 = do
      let (sufx, w35) = splitVarWord40 w40
      let w8 = pfx .|. sufx
      if w35 == VarWord35 0 then putWord8 w8 else putWord8 (w8 .|. 0x20) >> Bin.put w35

-- | Entries in the database are can be indexed for faster access times. Variable-sized element
-- types are followed by a 'VarWord35' blob-size.
getIndexElem :: Get RowTag
getIndexElem = do
  w8 <- Bin.getWord8
  let mtyp    = (.&. 0x03) . flip shift (-6) $ w8 -- get the 8th and 7th bits
  let primTyp = toEnum $ fromIntegral $ w8 .&. 0x3F -- mask off 6th through 1st bits
  let isLong  = w8 .&. 0x20 /= 0 -- check the 6th bit (orthogonal to primType)
  let getLong constr = constr . varWord40 w8 <$> if isLong then Bin.get else pure (VarWord35 0)
  case mtyp of
    0 -> pure $ NumericPrimitive primTyp
    1 -> HomoArray primTyp <$> Bin.get
    2 -> getLong UTF8String
    3 -> getLong BinaryBlob
    _ -> fail $ "INTERNAL ERROR: getIndexElem -- deserialized "++show mtyp++
                " which is an invalid index meta-type value."

----------------------------------------------------------------------------------------------------

calcByteSize :: (a -> Put) -> a -> Int
calcByteSize put = fromIntegral . checkedLength . runPut . put where
  checkedLength = BLazy.length >>> \ i -> let max = maxBound :: Int in
    if i <= fromIntegral max then i else error $
      "'calcByteSize': numeric primitive will be serialized to an object of "++show i++
      " bytes of binary data, a row can only contain "++show max++" bytes"

primTypeUTFCharSize :: Int
primTypeUTFCharSize = 3

-- | The class of type that can be represented as a single 'PrimElemType' when serialized.
class IsNumericPrimitive a where
  numericPrimitiveType :: Proxy a -> PrimElemType
  numericPrimitiveSize :: Proxy a -> Int

instance IsNumericPrimitive UTFChar where
  numericPrimitiveType Proxy = PrimTypeUTFChar
  numericPrimitiveSize Proxy = primTypeUTFCharSize

instance IsNumericPrimitive Int where
  numericPrimitiveType Proxy = PrimTypeHostInt
  numericPrimitiveSize Proxy = calcByteSize putInthost 0

instance IsNumericPrimitive Int8 where
  numericPrimitiveType Proxy = PrimTypeInt8
  numericPrimitiveSize Proxy = calcByteSize putInt8 0

instance IsNumericPrimitive Int16 where
  numericPrimitiveType Proxy = PrimTypeInt16
  numericPrimitiveSize Proxy = calcByteSize putInt16host 0

instance IsNumericPrimitive Int32 where
  numericPrimitiveType Proxy = PrimTypeInt32
  numericPrimitiveSize Proxy = calcByteSize putInt32host 0

instance IsNumericPrimitive Int64 where
  numericPrimitiveType Proxy = PrimTypeInt64
  numericPrimitiveSize Proxy = calcByteSize putInt64host 0

instance IsNumericPrimitive Word where
  numericPrimitiveType Proxy = PrimTypeHostWord
  numericPrimitiveSize Proxy = calcByteSize putWordhost 0

instance IsNumericPrimitive Word8 where
  numericPrimitiveType Proxy = PrimTypeWord8
  numericPrimitiveSize Proxy = calcByteSize putWord8 0

instance IsNumericPrimitive Word16 where
  numericPrimitiveType Proxy = PrimTypeWord16
  numericPrimitiveSize Proxy = calcByteSize putWord16host 0

instance IsNumericPrimitive Word32 where
  numericPrimitiveType Proxy = PrimTypeWord32
  numericPrimitiveSize Proxy = calcByteSize putWord32host 0

instance IsNumericPrimitive Word64 where
  numericPrimitiveType Proxy = PrimTypeWord64
  numericPrimitiveSize Proxy = calcByteSize putWord64host 0

instance IsNumericPrimitive Float where
  numericPrimitiveType Proxy = PrimTypeFloat
  numericPrimitiveSize Proxy = calcByteSize putFloathost 0

instance IsNumericPrimitive Double where
  numericPrimitiveType Proxy = PrimTypeDouble
  numericPrimitiveSize Proxy = calcByteSize putDoublehost 0

----------------------------------------------------------------------------------------------------

-- | This is a vector with elements packed into a strict 'BStrict.ByteString'.
newtype PackedVector elem = PackedVector { toByteString :: BStrict.ByteString }
  deriving (Eq, Ord)

instance Semigroup (PackedVector elem) where
  (PackedVector a) <> (PackedVector b) = PackedVector (a <> b)

instance Monoid (PackedVector elem) where
  mempty = PackedVector mempty
  mappend = (<>)

(!?) :: forall elem
     . (IsNumericPrimitive elem, Bin.Binary elem)
     => PackedVector elem -> Int -> Maybe elem
(!?) (PackedVector bytes) i = case decoder of
  Done _ _ a -> Just a
  _          -> Nothing
  where
    u = fromIntegral $ numericPrimitiveSize (Proxy :: Proxy elem)
    decoder = pushChunk (runGetIncremental $ isolate u Bin.get) $
      BStrict.take u $ BStrict.drop (i * u) bytes

(!) :: (IsNumericPrimitive elem, Bin.Binary elem) => PackedVector elem -> Int -> elem
(!) vec i = maybe (error msg) id $ vec !? i where
  msg = "PackedVector of length "++show (ProcGen.TinyRelDB.length vec)++" indexed with "++show i

length :: forall elem . IsNumericPrimitive elem => PackedVector elem -> Int
length (PackedVector bytes) = BStrict.length bytes `div`
  (fromIntegral $ numericPrimitiveSize (Proxy :: Proxy elem))

fromList :: forall elem . (IsNumericPrimitive elem, Bin.Binary elem) => [elem] -> PackedVector elem
fromList elems = PackedVector $ BLazy.toStrict $ runPut (mapM_ Bin.put elems)

toList :: forall elem . (IsNumericPrimitive elem, Bin.Binary elem) => PackedVector elem -> [elem]
toList (PackedVector bytes) = loop  bytes where
  u = fromIntegral $ numericPrimitiveSize (Proxy :: Proxy elem)
  loop bytes = case runGetIncremental (isolate u Bin.get) `pushChunk` bytes of
    Done bytes _ a -> a : loop bytes
    _              -> []

----------------------------------------------------------------------------------------------------

class IsPrimitive a where
  -- | A 'Data.Binary.Put.Put'-ter that does the work of actually serializing the value of @a@.
  -- Returns a 'PrimElemType' that reflects the type @a@.
  putPrimitive  :: a -> PutM RowTag
  -- | A 'Data.Binary.Get.Get'-ter that does the work of deserializing value of type @a@ assuming
  -- the 'PrimElemType' matches the type element currently under the cursor in the header of the
  -- 'TaggedRow'.
  getPrimitive  :: RowTag -> Get a

instance IsPrimitive UTFChar   where
  putPrimitive   = putNumericPrimitive putUTFChar
  getPrimitive _ = getUTFChar

instance IsPrimitive Int    where
  putPrimitive   = putNumericPrimitive putInthost
  getPrimitive _ = getInthost

instance IsPrimitive Int8   where
  putPrimitive   = putNumericPrimitive putInt8
  getPrimitive _ = getInt8

instance IsPrimitive Int16  where
  putPrimitive   = putNumericPrimitive putInt16le
  getPrimitive _ = getInt16le

instance IsPrimitive Int32  where
  putPrimitive   = putNumericPrimitive putInt32le
  getPrimitive _ = getInt32le

instance IsPrimitive Int64  where
  putPrimitive   = putNumericPrimitive putInt64le
  getPrimitive _ = getInt64le

instance IsPrimitive Word   where
  putPrimitive   = putNumericPrimitive putWordhost
  getPrimitive _ = getWordhost

instance IsPrimitive Word8  where
  putPrimitive   = putNumericPrimitive putWord8
  getPrimitive _ = getWord8

instance IsPrimitive Word16 where
  putPrimitive   = putNumericPrimitive putWord16le
  getPrimitive _ = getWord16le

instance IsPrimitive Word32 where
  putPrimitive   = putNumericPrimitive putWord32le
  getPrimitive _ = getWord32le

instance IsPrimitive Word64 where
  putPrimitive   = putNumericPrimitive putWord64le
  getPrimitive _ = getWord64le

instance IsPrimitive Float  where
  putPrimitive   = putNumericPrimitive putFloatle
  getPrimitive _ = getFloatle

instance IsPrimitive Double where
  putPrimitive   = putNumericPrimitive putDoublele
  getPrimitive _ = getDoublele

instance IsPrimitive String where
  putPrimitive str = do
    let bin = runPut $ putStringUtf8 str
    putLazyByteString bin
    return $ UTF8String $ varWord40Length "String" $ BLazy.length bin
  getPrimitive     = \ case
    UTF8String (VarWord40 siz) -> UTF8Lazy.toString <$> getLazyByteString siz
    _                          -> mzero

instance IsPrimitive TLazy.Text where
  putPrimitive = putPrimitive . TLazy.unpack
  getPrimitive = fmap TLazy.pack . getPrimitive

instance IsPrimitive TStrict.Text where
  putPrimitive = putPrimitive . TStrict.unpack
  getPrimitive = fmap TStrict.pack . getPrimitive

instance IsNumericPrimitive elem => IsPrimitive (PackedVector elem) where
  putPrimitive (PackedVector bytes) = do
    putByteString bytes
    let (VarWord35 max) = maxBound
        len = fromIntegral (BStrict.length bytes) `div`
          (numericPrimitiveSize (Proxy :: Proxy elem))
    if fromIntegral len <= max
     then return $ HomoArray (numericPrimitiveType (Proxy :: Proxy elem)) $
                      VarWord35 (fromIntegral len)
     else error $ "'ProcGen.TinyRelDB.putPrimitive' cannot put array with "++show len++
                  " elements, maximum number of elements is "++show max
  getPrimitive = \ case
    HomoArray typ (VarWord35 len) | typ == numericPrimitiveType (Proxy :: Proxy elem) ->
      PackedVector . BLazy.toStrict <$>
      getLazyByteString (len * fromIntegral (numericPrimitiveSize (Proxy :: Proxy elem)))
    _ -> mzero

instance IsPrimitive BLazy.ByteString where
  putPrimitive bytes = do
    putLazyByteString bytes
    let len = BLazy.length bytes
    let (VarWord40 max) = maxBound
    if len <= max
     then return $ BinaryBlob $ VarWord40 len
     else error $ "'ProcGen.TinyRelDB.putPrimitive' cannot put binary blob with "++show len++
                  " elements, maximum number of elements is "++show max
  getPrimitive = \ case
    BinaryBlob (VarWord40 len) -> getLazyByteString len
    _ -> mzero

instance IsPrimitive BStrict.ByteString where
  putPrimitive = putPrimitive . BLazy.fromStrict
  getPrimitive = fmap BLazy.toStrict . getPrimitive

varWord40Length :: String -> Int64 -> VarWord40
varWord40Length typmsg len =
  let (VarWord40 max) = maxBound
  in  if VarWord40 len <= maxBound then VarWord40 len else error $
        "A "++typmsg++" of "++show len++
        " bytes is too large to be serialized by TinyRelDB (max size is "++show max++")"

-- | Like 'indexElemType' but only works for any type that instantiates 'IsNumericPrimitive', and
-- always produces a value of 'NumericPrimitive'.
putNumericPrimitive :: forall a . IsNumericPrimitive a => (a -> Put) -> a -> PutM RowTag
putNumericPrimitive put a =
  put a >> return (NumericPrimitive $ numericPrimitiveType (Proxy :: Proxy a))

----------------------------------------------------------------------------------------------------

-- | This data type expresses a 'TaggedRow' element in compressed form. It is byte string lazily
-- copied from the 'TaggedRow's serialized byte string.
newtype RowTagBytes = RowTagBytes { rowElemBytes :: BLazy.ByteString }
  deriving (Eq, Ord)

type RowElemOffset = Int
type RowElemLength = Int

data RowHeaderElem
  = RowHeaderElem
    { headerItemType   :: RowTag
    , headerItemOffset :: RowElemOffset
    , headerItemLength :: RowElemLength
    }
  deriving (Eq, Ord)

type RowHeaderTable = [(Int, RowHeaderElem)]

-- | A serialized row header contains several 'RowTag's, each one of variable size. This
-- 'RowHeader' type is an un-serialized form of the row header, where each 'RowTag' takes 6
-- bytes regardless of it's value. This allows for faster indexing and decoding.
data RowHeader
  = RowHeader
    { rowHeaderLength   :: !RowElemLength
      -- ^ Get the number of elements in this 'Row'.
    , rowHeaderByteSize :: !RowElemOffset
    , rowHeaderTable    :: RowHeaderTable
    }
  deriving (Eq, Ord)

-- | Used for error reporting when a 'RowHeaderElem' could be reported but won't be because the
-- 'Select' statement cursor went past the last item in the row.
_maybe_off :: SelectEnv -> Maybe RowHeaderElem -> RowElemOffset
_maybe_off (SelectEnv{selectRow=(TaggedRow bytes), selectRowHeader=hdr}) =
  maybe (BStrict.length bytes) (((rowHeaderByteSize hdr) +) . headerItemOffset)

getRowHeaderElem :: Get RowHeaderElem
getRowHeaderElem = do
   item <- Bin.get
   let siz  = indexElemTypeSize item 
   return RowHeaderElem{ headerItemType=item, headerItemOffset=0, headerItemLength=siz }

getRowHeader :: Get (RowHeader, BLazy.ByteString)
getRowHeader = Bin.get >>= \ (VarWord35 w) -> loop w w 0 id where
  loop w nelems off elems = if nelems <= 0
    then do
      hdrsiz <- bytesRead
      bytes  <- getRemainingLazyByteString
      return
        ( RowHeader
          { rowHeaderLength   = fromIntegral w
          , rowHeaderByteSize = fromIntegral hdrsiz -- TODO: add a (Int64 -> Int) check here?
          , rowHeaderTable    = zip [0 ..] $ elems []
          }
        , bytes
        )
    else do
     elem <- getRowHeaderElem
     loop w (nelems - 1) (off + headerItemLength elem) $ elems . ((elem{headerItemOffset = off}) :)

putRowHeader :: RowHeader -> Put
putRowHeader h = do
  let (VarWord35 max) = maxBound
  let len             = rowHeaderLength h
  let bytelen         = sum $ headerItemLength . snd <$> rowHeaderTable h
  if fromIntegral len <= max
   then
    if fromIntegral bytelen <= max
     then do
        binPutVarWord35 $ VarWord35 $ fromIntegral len
        mapM_ (Bin.put . headerItemType . snd) $ rowHeaderTable h
     else error $ "'ProcGen.TinyRelDB.RowHeader' content would require "++show bytelen++
                  "bytes, maximum allowable content size is "++show max++" bytes"
   else error $ "'ProcGen.TinyRelDB.RowHeader' contains "++show len++" elements, "++
                "the maximum allowable number of elements is "++show max

-- | Split the 'RowHeader' from a 'TaggedRow' and return the header along with the content without
-- inspecting the content or performing any deserialization. Use this if you want just the header
-- alone.
rowHeader :: TaggedRow -> (RowHeader, BLazy.ByteString)
rowHeader (TaggedRow bytes) =
  if BStrict.null bytes
   then (emptyHeader, mempty)
   else runGet getRowHeader $ BLazy.fromStrict bytes
  where
    emptyHeader = RowHeader
      { rowHeaderLength   = 0
      , rowHeaderByteSize = 0
      , rowHeaderTable    = []
      }

-- | Where N is an 'Int', obtain the N'th index in the 'RowHeader', along with the index in the
-- byte string at which the serialized object is stored, and the stored object size.
indexHeaderTag :: RowHeader -> Int -> Select RowHeaderElem
indexHeaderTag hdr i = maybe mzero return $ lookup i $ rowHeaderTable hdr

----------------------------------------------------------------------------------------------------

-- | Inspect a 'TaggedRow' header (the list of 'RowTag's that mark the size of each serialized
-- elements) and use this to decide whether this TaggedRow should be decoded. Then deserialize the
-- relevant elements to construct a value of type @a@.
newtype Select a
  = Select
    { unwrapSelect ::
        ReaderT SelectEnv (StateT RowHeaderTable (Except SelectAnomaly)) a
    }
  deriving (Functor, Applicative, Monad)

data SelectEnv
  = SelectEnv
    { selectRowHeader :: RowHeader
    , selectRow       :: TaggedRow
    }
  deriving (Eq, Ord)

instance Show SelectEnv where
  show env@(SelectEnv{selectRowHeader=hdr}) = unlines
    ( "'TaggedRow' -> 'RowHeader' contents:" :
      "  index   offset   length type"       : do
        (i,RowHeaderElem{headerItemType=typ,headerItemOffset=off,headerItemLength=len}) <-
          rowHeaderTable hdr
        [printf "  %5i %8i %8i %s" i off len $ show typ]
     ) ++ "Hex dump of the whole 'TaggedRow', including bytes of the 'RowHeader':\n"
       ++ hexDumpTaggedRow env Nothing 

data SelectAnomaly
  = SelectUnmatched
    -- ^ Throwing this exception indicates to simply ignore this row because it does not match.
  | PrematureEndOfRow
    { corruptSelectEnv   :: !SelectEnv
    , corruptRowReason   :: !TStrict.Text
    } -- ^ This exception is thrown by 'throwUnlessRowEnd', indicates that the 'TaggedRow' should
      -- have contained data that was expected by the 'Select' function which throws this error.
  | CorruptRow
    { corruptSelectEnv   :: !SelectEnv
      -- ^ a copy of the 'RowHeader' and 'TaggedRow' that was being used by the 'Select' statement
      -- that threw this exception.
    , corruptHeaderIndex :: !Int
      -- ^ The index of the 'corruptHeaderElem'
    , corruptHeaderElem  :: Maybe RowHeaderElem
      -- ^ Which 'RowHeaderElem' in the 'rowHeaderTable' was being scrutinized by the 'Select'
      -- statement when it threw this exception. This is set to 'Prelude.Nothing' if all tag
      -- elements were consumed.
    , corruptRowReason   :: !TStrict.Text
    } -- ^ This exception is used when there seems to be something wrong with the data, and the
      -- 'Select' function should halt immediately.
  | UnknownError
    { corruptSelectEnv   :: !SelectEnv
    , corruptRowReason   :: !TStrict.Text
    } -- ^ A catch-all exception.
  deriving (Eq, Ord)

instance Show SelectAnomaly where
  show = \ case
    SelectUnmatched            -> "Row does not match the evaluated 'Select' function."
    PrematureEndOfRow _env msg -> "Failed to parse row: "++TStrict.unpack msg
    UnknownError      _env msg -> "Failed to select elements from row: "++TStrict.unpack msg
    CorruptRow  env i elem msg -> "Row appears to be corrupted (index="++
      show i++", headerSize="++show (rowHeaderLength $ selectRowHeader env)++", offset="++
      show (_maybe_off env elem)++"):\n"++TStrict.unpack msg++"\n"

instance MonadFail Select where
  fail msg = do
    env <- Select ask
    throwError $ UnknownError env $ TStrict.pack msg

instance MonadError SelectAnomaly Select where
  throwError = Select . lift . lift . throwError
  catchError (Select try) catch = Select $ catchError try $ unwrapSelect . catch

instance MonadPlus Select where
  mzero = Select $ lift $ lift $ throwError SelectUnmatched
  mplus a b = catchError a $ \ case
    SelectUnmatched -> b
    err              -> throwError err

instance MonadReader SelectEnv Select where
  ask = Select ask
  local f (Select m) = Select $ local f m
  
instance MonadState RowHeaderTable Select where
  state = Select . lift . state

instance                Alternative Select    where { empty = mzero; (<|>) = mplus; }
instance Semigroup a => Semigroup  (Select a) where { a <> b = (<>) <$> a <*> b; }
instance Monoid    a => Monoid     (Select a) where
  mempty      = pure mempty
  mappend a b = mappend <$> a <*> b

-- | Construct a 'SelectEnv' from a 'TaggedRow'. Use the result of this function to evaluate
-- 'runRowSelect'.
selectEnv :: TaggedRow -> SelectEnv
selectEnv row = SelectEnv{ selectRowHeader = header, selectRow = row } where
  (header, _ ) = rowHeader row

runRowSelect :: Select a -> SelectEnv -> Either SelectAnomaly a
runRowSelect (Select f) env = runExcept $
  evalStateT (runReaderT f env) $ rowHeaderTable $ selectRowHeader env

-- | Return a pair, the number of elements selected so far, and the number of elements that this row
-- contains.
selectedElemCount :: Select (Int, Int)
selectedElemCount = Select $ do
  count <- asks (rowHeaderLength . selectRowHeader)
  get >>= \ case
    []       -> pure (count, count)
    (i, _):_ -> pure (i    , count)

-- | Step to the next 'RowTagBytes', is 'Control.Monad.mzero' if there are no elements left.
nextRowTag :: Select ()
nextRowTag = Select get >>= \ case { [] -> mzero; _:elems -> put elems; }

-- | Skip to the 'RowTagBytes' matching the given predicate.
skipToRowElem :: (RowTag -> Bool) -> Select RowHeaderElem
skipToRowElem found = join $ Select $ gets $
  dropWhile (not . found . headerItemType . snd) >>> \ case
    []           -> mzero
    (_, e):elems -> state $ const (e, elems)

-- | Throw an exception unless we have consumed all of the 'RowTagBytes's by this point. If we have
-- consumed every item in this 'TaggedRow', this function does nothing and throws no exception.
throwUnlessRowEnd :: TStrict.Text -> Select ()
throwUnlessRowEnd msg = Select get >>= \ case
  [] -> pure ()
  _  -> do
    env <- Select ask
    throwError PrematureEndOfRow{ corruptSelectEnv = env, corruptRowReason = msg }

-- | Throw a 'CorruptRow' exception with the cursor information and a brief message.
throwCorruptRow :: TStrict.Text -> Select void
throwCorruptRow msg = do
  env <- Select ask
  let hdr = selectRowHeader env
  let len = rowHeaderLength hdr
  (i, elem) <- Select $ lift $ gets $ \ case
    []          -> (len, Nothing)
    (i, elem):_ -> (i  , Just elem)
  throwError $ CorruptRow env i elem msg

-- | Extract the current 'RowTagBytes' under the cursor, then advance the cursor.
rowElem :: Select (RowTag, RowTagBytes)
rowElem = Select get >>= \ case
  []       -> mzero
  (_ , RowHeaderElem
       { headerItemType   = idxelem
       , headerItemOffset = off
       , headerItemLength = siz
       }
   ):elems -> do
    Select (put elems)
    env <- ask
    let (TaggedRow bytes) = selectRow env
    let hdrsiz = rowHeaderByteSize $ selectRowHeader env
    return
      ( idxelem
      , RowTagBytes $ BLazy.fromStrict $
          BStrict.take (hdrsiz + fromIntegral siz) $
          BStrict.drop (hdrsiz + fromIntegral off) bytes
      )

-- | Use a 'Data.Binary.Get.Get' function from the "Data.Binary.Get" module to unpack the current
-- row element retrieved from 'rowElem'.
unpackRowElem :: (RowTag -> Get a) -> Select (RowTag, a)
unpackRowElem unpack = do
  (idx, RowTagBytes bytes) <- rowElem
  let max = fromIntegral (maxBound :: Int) :: Int64
      len = indexElemTypeSize idx
      err = error $
        "Serialized object of type "++show idx++" has binary size of "++show len++
        " bytes, but decoder can only consume "++show max++
        " bytes at a time"
  if fromIntegral len > max then err else
    case runGetOrFail (isolate (fromIntegral len) $ unpack idx) bytes of
      Right (_, _, a) -> return (idx, a)
      Left{}          -> mzero

-- | Match the value of any type @a@ that instantiates 'IsPrimitive' with the current 'rowElem'
-- under the cursor.
readRowPrim :: forall a . IsPrimitive a => Select a
readRowPrim = snd <$> unpackRowElem getPrimitive

-- | Obtain a copy of the 'TaggedRow' object currently being inspected by this 'Select' function.
currentTaggedRow :: Select TaggedRow
currentTaggedRow = asks selectRow

-- | If a 'SelectAnomaly' exception is raised, and if the 'SelectAnomaly' is a 'CorruptRow', this
-- function will produce a hex-dump of the content of the row, indicating the cursor position at
-- which the failure occured.
hexDumpIfCorruptRow :: SelectAnomaly -> String
hexDumpIfCorruptRow = \ case
  CorruptRow env _i elem _msg -> hexDumpTaggedRow env elem
  _  -> ""

----------------------------------------------------------------------------------------------------

-- | The type @'RowBuilderM' ()@ is used often so it helps to have a type synonym for it.
type RowBuilder = RowBuilderM ()

-- | This is a function for constructing 'TaggedRow's. The 'writeRowPrim' function works like the
-- serializing function 'Bin.put', and the 'RowBuilderM' function type works similar to the
-- 'Bin.Put' function type. The difference is that the type that is serialized must be a type in the
-- class 'IsPrimitive'. Evaluating this function with 'buildRow' will produce a 'TaggedRow'.
newtype RowBuilderM a = RowBuilderM (StateT BuildRowState PutM a)
  deriving (Functor, Applicative, Monad)

data BuildRowState
  = BuildRowState
    { buildElemCount  :: !Int
    , buildRowHeader  :: Builder
    }

instance Semigroup BuildRowState where
  (<>) (BuildRowState{buildElemCount=a,buildRowHeader=hdrA})
       (BuildRowState{buildElemCount=b,buildRowHeader=hdrB}) = BuildRowState
        { buildElemCount =    a  +    b
        , buildRowHeader = hdrA <> hdrB
        }

instance Monoid BuildRowState where
  mempty  = BuildRowState{ buildElemCount = 0, buildRowHeader = mempty }
  mappend = (<>)

-- | Obtain the number of elements that have been written prior to evaluating this function.
writtenElemCount :: RowBuilderM Int
writtenElemCount = RowBuilderM $ gets buildElemCount

execRowBuilder :: RowBuilder -> TaggedRow
execRowBuilder = snd . runRowBuilder

runRowBuilder :: RowBuilderM a -> (a, TaggedRow)
runRowBuilder (RowBuilderM f) = (a, newRow) where
  ((a, st), content) = runPutM $ runStateT f BuildRowState
    { buildElemCount = 0
    , buildRowHeader = mempty
    }
  newRow = TaggedRow $ BLazy.toStrict $ runPut $ do
    let len = fromIntegral $ buildElemCount st
    let (VarWord35 max) = maxBound
    if len > max
     then error $
      "'RowBuilder' constructed row with "++show len++
      " elements, but a maximum of "++show max++
      " elements is the limit."
     else do
      binPutVarWord35 $ VarWord35 len
      putBuilder $ buildRowHeader st
      putLazyByteString content

-- | Use the 'IsPrimitive' instance for data of type @a@ to store the information in @a@ into a
-- 'RowBuilder'.
writeRowPrim :: forall a . IsPrimitive a => a -> RowBuilder
writeRowPrim = writeRowPrimWith putPrimitive 

writeRowPrimWith :: (a -> PutM RowTag) -> a -> RowBuilder
writeRowPrimWith putPrim a = RowBuilderM $ do
  idxelem <- lift $ putPrim a
  modify $ \ st ->
    st{ buildElemCount  = buildElemCount  st + 1
      , buildRowHeader  = buildRowHeader  st <> execPut (putIndexElem idxelem)
      }

----------------------------------------------------------------------------------------------------

-- | The class of things that map to and from 'TaggedRow's in the Tiny Relation Database. This class
-- combines both query and update functions together. When performing queries and updates on a
-- 'Table' of 'TaggedRow's, store values of types in the class 'DBMapped' into the rows. If you
-- don't want 'TaggedRow's, if you instead want 'PlainRow's, store values of types in the class
-- 'Bin.Binary'.
class DBMapped a where
  writeRow :: a -> RowBuilder
  readRow  :: Select a

instance DBMapped UTFChar  where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Int      where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Int8     where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Int16    where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Int32    where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Int64    where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Word     where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Word8    where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Word16   where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Word32   where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Float    where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Double   where { writeRow = writeRowPrim; readRow = readRowPrim; }

instance DBMapped TaggedRow where
  writeRow (TaggedRow bytes) = writeRowPrim bytes
  readRow = TaggedRow <$> readRowPrim

-- | A UTF8-encoded 'UTF8Lazy.ByteString' is just a plain lazy 'BLazy.ByteString', but it can be
-- encoded with a different 'RowTag' type tag so that any string will match it when using a 'Select'
-- statement. Use this function to encode a lazy 'BLazy.ByteString' with a string tag in the
-- 'RowHeader' of a 'TaggedRow'. Use 'getPrimLazyUTF8String' to retrieve the exact data you put in.
putPrimLazyUTF8String :: UTF8Lazy.ByteString -> RowBuilder
putPrimLazyUTF8String = writeRowPrimWith $ \ str -> do
  putLazyByteString str
  return $ UTF8String $ varWord40Length "Lazy UTF8 ByteString" $
    BLazy.length str -- NOTE: this must be 'BLazy.length', and NOT 'UTF8Lazy.length'

-- | Retrieve a string object from a 'TaggedRow' as a lazy 'BLazy.ByteString'.
getPrimLazyUTF8String :: Select UTF8Lazy.ByteString
getPrimLazyUTF8String = fmap snd $ unpackRowElem $ \ case
  UTF8String (VarWord40 siz) -> getLazyByteString siz
  _                          -> mzero

-- | A UTF8-encoded 'UTF8Strict.ByteString' is just a plain strict 'BStrict.ByteString', but it can be
-- encoded with a different 'RowTag' type tag so that any string will match it when using a 'Select'
-- statement. Use this function to encode a strict 'BStrict.ByteString' with a string tag in the
-- 'RowHeader' of a 'TaggedRow'. Use 'getPrimStrictUTF8String' to retrieve the exact data you put in.
putPrimStrictUTF8String :: UTF8Strict.ByteString -> RowBuilder
putPrimStrictUTF8String = writeRowPrimWith $ \ str -> do
  putByteString str
  return $ UTF8String $ varWord40Length "Strict UTF8 ByteString" $ fromIntegral $
    BStrict.length str -- NOTE: this must be 'BStrict.length', and NOT 'UTF8Strict.length'

-- | Retrieve a string object from a 'TaggedRow' as a strict 'BStrict.ByteString'.
getPrimStrictUTF8String :: Select UTF8Strict.ByteString
getPrimStrictUTF8String = fmap snd $ unpackRowElem $ \ case
  UTF8String (VarWord40 siz) -> getByteString $ fromIntegral siz
  _                          -> mzero

putPrimUnboxedVector :: (IsNumericPrimitive elem, Unbox elem) => Unboxed.Vector elem -> RowBuilder
putPrimUnboxedVector = error "TODO"

getPrimUnboxedVector :: (IsNumericPrimitive elem, Unbox elem) => Select (Unboxed.Vector elem)
getPrimUnboxedVector = error "TODO"

----------------------------------------------------------------------------------------------------

-- | When using a database of 'BStrict.ByteString's where each 'BStrict.ByteString' contains
-- multiple serialized elements, selecting an arbitrary element from this row is an @O(n*m)@ time
-- complexity problem, where @n@ is the number of elements, and @m@ is the serialized size of each
-- element.
--
-- A 'TaggedRow' is a 'BStrict.ByteString' that has each element in the byte string tagged and
-- stored in a 'RowHeader'. The 'RowHeader' is also serialized and prepended to the
-- 'BStrict.ByteString' row data. Each tag in the 'RowHeader' contains the position and size of each
-- element in the 'BStrict.ByteString'. Finding the tag is an @O(n)@ complexity operation, but
-- selecting the object associated with the tag is an @O(1)@ complexity operation.
--
-- Thus, with a 'TaggedRow', selecting an element from the row is an @O(n*1) = O(n)@ complexity
-- operation, as opposed to using an untagged 'PlainRow' in which selecting an element is a @O(n*m)@
-- time complexity operation.
newtype TaggedRow = TaggedRow { taggedRowBytes :: BStrict.ByteString }
  deriving (Eq, Ord)

-- | A 'PlainRow' is an un-taggeed table row, which is an alternative table storage format to
-- 'TaggedRow'. Use a 'PlainRow' if you are sure you will view every element in the row every time
-- the row is inspected.
type PlainRow = BStrict.ByteString

-- | A table of stored elements. The @format@ of the 'Table' is either a 'TaggedRow' or a
-- 'PlainRow'.
newtype Table format = Table{ tableToSequence :: S.Seq format }
  deriving (Eq, Ord, Functor)

instance Semigroup (Table format) where { (Table a) <> (Table b) = Table (a <> b); }
instance Monoid    (Table format) where { mempty = Table mempty; mappend = (<>); }

instance Bin.Binary TaggedRow where
  put (TaggedRow bytes) = putByteString bytes
  get = do
    (header, bytes) <- getRowHeader
    return $ TaggedRow $ BLazy.toStrict $ runPut (putRowHeader header) <> bytes

hexDumpTaggedRow :: SelectEnv -> Maybe RowHeaderElem -> String
hexDumpTaggedRow env@(SelectEnv{selectRow=(TaggedRow bytes)}) elem = unlines $
  loop (0 :: Int64) (_maybe_off env elem) (BStrict.unpack bytes) where
    bytesPerRow = 16
    alignNumR i = (++ (show i)) $ case i of
      i | i < 10 -> "       "
      i | i < 100 -> "      "
      i | i < 1000 -> "     "
      i | i < 10000 -> "    "
      i | i < 100000 -> "   "
      i | i < 1000000 -> "  "
      i | i < 10000000 -> " "
      _                 -> ""
    loop grpnum i = splitAt bytesPerRow >>> \ case
      ([]   , []       ) -> []
      (group, remainder) ->
        ( alignNumR grpnum ++ '\x3A' :
          (group >>= \ c -> ' ' : (if c < 0x10 then ('0' :) else id) (showHex c ""))
         ) :
        (if i >= bytesPerRow then id else
            (("        : " ++ replicate (fromIntegral i * 3) ' ' ++ "^\\cursor") :)
         ) (((loop $! grpnum + bytesPerRow) $! i - bytesPerRow) remainder)

-- | Fold from the top of the table to the bottom. This function is defined in terms of
-- 'Data.Foldable.foldl'. The internal table type is a 'Data.Sequence.Seq' so runtime performance of
-- both 'tableFoldUp' and 'tableFoldDown' are almost always the same.
tableFoldDown
  :: (format -> Maybe a)
  -> (fold -> a -> fold) -> fold -> Table format -> fold
tableFoldDown get fold init (Table seq) =
  foldl (\ a bytes -> maybe a (fold a) $ get bytes) init seq

-- | Fold from the bottom of the table to the top. This function is defined in terms of
-- 'Data.Foldable.foldr'. The internal table type is a 'Data.Sequence.Seq' so runtime performance of
-- both 'tableFoldUp' and 'tableFoldDown' are almost always the same.
tableFoldUp
  :: (format -> Maybe a)
  -> (a -> fold -> fold) -> fold -> Table format -> fold
tableFoldUp get fold init (Table seq) =
  foldr (\ bytes init -> maybe init (`fold` init) $ get bytes) init seq
