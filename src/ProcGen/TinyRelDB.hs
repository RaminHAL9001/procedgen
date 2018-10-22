-- | A small relational database engine built-in to ProcGen so there is no need for a dependency on
-- Sqlite or any Haskell relational database mappers. The database is not entirely relational, and
-- really only provides functionality similar to a simplified spreadsheet program that operates on
-- binary CSV files, or on no files at all, everything can be done in memory only. This turns out to
-- be a useful abstraction for pretty printers and text editors.
module ProcGen.TinyRelDB
  where
--  ( -- * Classes of Primitives
--    Str.IsString(..), IsNumericPrimitive(..), IsPrimitive(..),
--    putNumericPrimitive,
--    -- * Database Data Types
--    PlainRow, TaggedRow, Table, taggedRowBytes, tableToSequence,
--    -- * Mapping Haskell Data Types
--    DBMapped(..), Select, SelectAnomaly(..), SelectEnv(..), RowBuilder, RowBuilderM,
--    writeRowPrim, tableFoldDown, tableFoldUp,
--    nextRowTag, skipToRowElem, expectRowEndWith, rowElem, unpackRowElem, readRowPrim,
--    -- * Low-Level Row Formatting
--    RowTagBytes, rowElemBytes,
--    RowHeaderElem(..), RowElemOffset, RowElemLength, RowHeaderTable,
--    RowHeader, rowHeader, indexHeaderTag,
--    RowTag(..), indexElemTypeSize,
--    PrimElemType(..), primElemTypeSize,
--    aggregA, aggreg,
--    -- * Packed Vectors
--    PackedVectorElem, toByteString, (!?), (!), length, fromList, toList,
--  ) where

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
import qualified Data.Vector.Unboxed         as Unboxed
--import qualified Data.Vector.Unboxed.Mutable as Mutable
import           Data.Word

import ProcGen.PrimeNumbers
import Numeric

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

-- | A variable-length unsigned integer used to record binary blob sizes in element indicies. A
-- 'VarWord35' consists of up to 5*7 = 35 bits, and since 2^35 is 32 Gibibytes, this will cover all
-- practical blob sizes for the forseeable future. The serialized form of this number is big-endian
-- 'Data.Word.Word8' values with the highest significant bit of each 'Data.Word.Word8' set to 1 if
-- it is not the final value in the number. The remaining 7 bits (the septet) are the content of the
-- value. There are up to 5 septets, however many are necessary to store the full numerical value.
newtype VarWord35 = VarWord35 Int64
  deriving (Eq, Ord, Show)

instance Bounded VarWord35 where
  minBound = VarWord35 0
  maxBound = VarWord35 0x00000007FFFFFFFF

instance Bin.Binary VarWord35 where
  put = binPutVarWord35
  get = binGetVarWord35

binPutVarWord35 :: VarWord35 -> Bin.Put
binPutVarWord35 (VarWord35 w) =
  loop (a,b) $ loop (b,c) $ loop (c,d) $ loop (d,e) $ loop (e,0::Word8) $ pure ()
  where
    -- I'm going out of my way to tell the compiler to unroll this loop.
    s = fromIntegral . (.&. 0x7F) . shift w . negate . (* 7) :: Int -> Word8
    (a, b, c, d, e) = (s 4, s 3, s 2, s 1, s 0)
    loop (a, b) next = if b == 0 then Bin.putWord8 a else Bin.putWord8 (a .|. 0x80) >> next

binGetVarWord35 :: Bin.Get VarWord35
binGetVarWord35 = fmap VarWord35 $ loop $ loop $ loop $ loop $ loop $ pure 0 where
  loop next = do
    w8 <- Bin.getWord8
    let w64 = fromIntegral $ w8 .&. 0x7F
    if w8 .&. 0x80 == 0 then pure w64 else (.|.) (shift w64 7) <$> next

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
  deriving (Eq, Ord, Show)

instance Bin.Binary RowTag where
  put = putIndexElem
  get = getIndexElem

indexElemBinPrefix :: RowTag -> Word8
indexElemBinPrefix = (`shift` 6) . \ case
  NumericPrimitive{} -> 0
  HomoArray{}        -> 1
  UTF8String{}       -> 2
  BinaryBlob{}       -> 3

indexElemTypeSize :: RowTag -> Int64
indexElemTypeSize = \ case
  NumericPrimitive       e  -> primElemTypeSize e
  HomoArray e (VarWord35 x) -> primElemTypeSize e * x
  UTF8String  (VarWord40 w) -> w
  BinaryBlob  (VarWord40 w) -> w

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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

primElemTypeBinSuffix :: PrimElemType -> Word8
primElemTypeBinSuffix = fromIntegral . fromEnum

-- | The serialized size (in bytes) of a value represented by the 'PrimElemType'.
primElemTypeSize :: PrimElemType -> Int64
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

calcByteSize :: Bin.Binary a => a -> Int64
calcByteSize = fromIntegral . BLazy.length . runPut . Bin.put 

primTypeUTFCharSize :: Int64
primTypeUTFCharSize = 3

-- | The class of type that can be represented as a single 'PrimElemType' when serialized.
class IsNumericPrimitive a where
  numericPrimitiveType :: Proxy a -> PrimElemType
  numericPrimitiveSize :: Proxy a -> Int64

instance IsNumericPrimitive Char where
  numericPrimitiveType Proxy = PrimTypeUTFChar
  numericPrimitiveSize Proxy = primTypeUTFCharSize

instance IsNumericPrimitive Int where
  numericPrimitiveType Proxy = PrimTypeHostInt
  numericPrimitiveSize Proxy = calcByteSize (0 :: Int)

instance IsNumericPrimitive Int8 where
  numericPrimitiveType Proxy = PrimTypeInt8
  numericPrimitiveSize Proxy = calcByteSize (0 :: Int8)

instance IsNumericPrimitive Int16 where
  numericPrimitiveType Proxy = PrimTypeInt16
  numericPrimitiveSize Proxy = calcByteSize (0 :: Int16)

instance IsNumericPrimitive Int32 where
  numericPrimitiveType Proxy = PrimTypeInt32
  numericPrimitiveSize Proxy = calcByteSize (0 :: Int32)

instance IsNumericPrimitive Int64 where
  numericPrimitiveType Proxy = PrimTypeInt64
  numericPrimitiveSize Proxy = calcByteSize (0 :: Int64)

instance IsNumericPrimitive Word where
  numericPrimitiveType Proxy = PrimTypeHostWord
  numericPrimitiveSize Proxy = calcByteSize (0 :: Word)

instance IsNumericPrimitive Word8 where
  numericPrimitiveType Proxy = PrimTypeWord8
  numericPrimitiveSize Proxy = calcByteSize (0 :: Word8)

instance IsNumericPrimitive Word16 where
  numericPrimitiveType Proxy = PrimTypeWord16
  numericPrimitiveSize Proxy = calcByteSize (0 :: Word16)

instance IsNumericPrimitive Word32 where
  numericPrimitiveType Proxy = PrimTypeWord32
  numericPrimitiveSize Proxy = calcByteSize (0 :: Word32)

instance IsNumericPrimitive Word64 where
  numericPrimitiveType Proxy = PrimTypeWord64
  numericPrimitiveSize Proxy = calcByteSize (0 :: Word64)

instance IsNumericPrimitive Float where
  numericPrimitiveType Proxy = PrimTypeFloat
  numericPrimitiveSize Proxy = calcByteSize (0 :: Float)

instance IsNumericPrimitive Double where
  numericPrimitiveType Proxy = PrimTypeDouble
  numericPrimitiveSize Proxy = calcByteSize (0 :: Double)

----------------------------------------------------------------------------------------------------

class PackedVectorElem elem where { elemUnitSize :: Proxy elem -> Int; }

-- | This is a vector with elements packed into a strict 'BStrict.ByteString'.
newtype PackedVector elem = PackedVector { toByteString :: BStrict.ByteString }
  deriving (Eq, Ord)

instance Semigroup (PackedVector elem) where
  (PackedVector a) <> (PackedVector b) = PackedVector (a <> b)

instance Monoid (PackedVector elem) where
  mempty = PackedVector mempty
  mappend = (<>)

(!?) :: forall elem
     . (PackedVectorElem elem, Bin.Binary elem)
     => PackedVector elem -> Int -> Maybe elem
(!?) (PackedVector bytes) i = case decoder of
  Done _ _ a -> Just a
  _          -> Nothing
  where
    u = elemUnitSize (Proxy :: Proxy elem)
    decoder = pushChunk (runGetIncremental $ isolate u Bin.get) $
      BStrict.take u $ BStrict.drop (i * u) bytes

(!) :: (PackedVectorElem elem, Bin.Binary elem) => PackedVector elem -> Int -> elem
(!) vec i = maybe (error msg) id $ vec !? i where
  msg = "PackedVector of length "++show (ProcGen.TinyRelDB.length vec)++" indexed with "++show i

length :: forall elem . PackedVectorElem elem => PackedVector elem -> Int
length (PackedVector bytes) = BStrict.length bytes `div` elemUnitSize (Proxy :: Proxy elem)

fromList :: forall elem . (PackedVectorElem elem, Bin.Binary elem) => [elem] -> PackedVector elem
fromList elems = PackedVector $ BLazy.toStrict $ runPut (mapM_ Bin.put elems)

toList :: forall elem . (PackedVectorElem elem, Bin.Binary elem) => PackedVector elem -> [elem]
toList (PackedVector bytes) = loop  bytes where
  u = elemUnitSize (Proxy :: Proxy elem)
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

instance IsPrimitive Char   where
  putPrimitive = putNumericPrimitive $ \ char -> do
    let c = ord char
    let w = fromIntegral . (.&. 0xFF) :: Int -> Word8
    b <- pure $ shift c 8
    a <- pure $ shift b 8
    putWord8 (w a) >> putWord8 (w b) >> putWord8 (w c)
  getPrimitive _ = do
    let w i = (`shift` i) . fromIntegral :: Word8 -> Int
    (\ a b c -> chr $ w 16 a * w 8 b * w 0 c) <$> getWord8 <*> getWord8 <*> getWord8

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

instance IsPrimitive UTF8Lazy.ByteString where
  putPrimitive str = do
    putLazyByteString str
    return $ UTF8String $ varWord40Length "Lazy UTF8 ByteString" $
      BLazy.length str -- NOTE: this must be 'BLazy.length', and NOT 'UTF8Lazy.length'
  getPrimitive     = \ case
    UTF8String (VarWord40 siz) -> getLazyByteString siz
    _                          -> mzero

instance IsPrimitive UTF8Strict.ByteString where
  putPrimitive str = do
    putByteString str
    return $ UTF8String $ varWord40Length "Strict UTF8 ByteString" $ fromIntegral $
      BStrict.length str -- NOTE: this must be 'BStrict.length', and NOT 'UTF8Strict.length'
  getPrimitive     = \ case
    UTF8String (VarWord40 siz) -> getByteString $ fromIntegral siz
    _                          -> mzero

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

-- TODO
-- TODO
-- instances for IsPrimitive unboxed vector types
-- instances for string types

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

type RowElemOffset = Int64
type RowElemLength = Int64

data RowHeaderElem
  = RowHeaderElem
    { headerItemType   :: RowTag
    , headerItemOffset :: RowElemOffset
    , headerItemLength :: RowElemLength
    }
  deriving (Eq, Ord)

type RowHeaderTable = [RowHeaderElem]

-- | A serialized row header contains several 'RowTag's, each one of variable size. This
-- 'RowHeader' type is an un-serialized form of the row header, where each 'RowTag' takes 6
-- bytes regardless of it's value. This allows for faster indexing and decoding.
data RowHeader
  = RowHeader
    { rowHeaderLength   :: !Int
      -- ^ Get the number of elements in this 'Row'.
    , rowHeaderByteSize :: !Word64
    , rowHeaderTable    :: RowHeaderTable
    }
  deriving (Eq, Ord)

-- | Split the 'RowHeader' from a 'TaggedRow' and return the header along with the content without
-- inspecting the content or performing any deserialization. Use this if you want just the header
-- alone.
rowHeader :: TaggedRow -> (RowHeader, BLazy.ByteString)
rowHeader (TaggedRow bytes) = if BStrict.null bytes then (emptyHeader, mempty) else init where
  emptyHeader = RowHeader
    { rowHeaderLength   = 0
    , rowHeaderByteSize = 0
    , rowHeaderTable    = []
    }
  init = runGet (Bin.get >>= \ (VarWord35 w) -> loop w w 0 id) (BLazy.fromStrict bytes)
  loop w nelems off elems = if nelems <= 0
    then do
      hdrsiz <- bytesRead
      bytes  <- getRemainingLazyByteString
      return
        ( RowHeader
          { rowHeaderLength   = fromIntegral w
          , rowHeaderByteSize = fromIntegral hdrsiz
          , rowHeaderTable    = elems []
          }
        , bytes
        )
    else do
     item <- Bin.get
     let siz  = indexElemTypeSize item + off
     let elem = RowHeaderElem{ headerItemType=item, headerItemOffset=off, headerItemLength=siz }
     loop w (nelems - 1) (off + siz) $ elems . (elem :)

-- | Where N is an 'Int', obtain the N'th index in the 'RowHeader', along with the index in the
-- byte string at which the serialized object is stored, and the stored object size.
indexHeaderTag :: RowHeader -> Int -> Select RowHeaderElem
indexHeaderTag hdr i = maybe mzero return $ lookup i $ zip [0 ..] $ rowHeaderTable hdr

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

data SelectAnomaly
  = SelectUnmatched
    -- ^ Ignore this row, it does not match.
  | CorruptRow !TaggedRow !TStrict.Text
    -- ^ There seems to be something wrong with the data, halt immediately.
  | UnknownError !TStrict.Text (Maybe TaggedRow)
    -- ^ A catch-all exception.
  deriving (Eq, Ord)

instance MonadFail Select where
  fail = throwError . flip UnknownError Nothing . TStrict.pack

instance MonadError SelectAnomaly Select where
  throwError = Select . lift . lift . throwError
  catchError (Select try) catch = Select $ catchError try $ unwrapSelect . catch

instance MonadPlus Select where
  mzero = Select $ lift $ lift $ throwError SelectUnmatched
  mplus a b = catchError a $ \ case
    SelectUnmatched -> b
    err             -> throwError err

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

-- | Step to the next 'RowTagBytes', is 'Control.Monad.mzero' if there are no elements left.
nextRowTag :: Select ()
nextRowTag = join $ gets $ \ case { [] -> mzero; _:elems -> put elems; }

-- | Skip to the 'RowTagBytes' matching the given predicate.
skipToRowElem :: (RowTag -> Bool) -> Select RowHeaderElem
skipToRowElem found = join $ gets $ dropWhile (not . found . headerItemType) >>> \ case
  []      -> mzero
  e:elems -> state $ const (e, elems)

-- | Fail unless we have consumed all of the 'RowTagBytes's by this point.
expectRowEndWith :: SelectAnomaly -> Select ()
expectRowEndWith err = Select (lift get) >>= \ case { [] -> pure (); _ -> throwError err; }

-- | Extract the current 'RowTagBytes' under the cursor, then advance the cursor.
rowElem :: Select (RowTag, RowTagBytes)
rowElem = join $ gets $ \ case
  []      -> mzero
  ( RowHeaderElem
    { headerItemType   =  idxelem
    , headerItemOffset = off
    , headerItemLength = siz
    }):elems -> do
        put elems
        (TaggedRow bytes) <- asks selectRow
        return
          ( idxelem
          , RowTagBytes $ BLazy.fromStrict $
              BStrict.take (fromIntegral siz) $
              BStrict.drop (fromIntegral off) bytes
          )

-- | Use a 'Data.Binary.Get.Get' function from the "Data.Binary.Get" module to unpack the current
-- row element retrieved from 'rowElem'.
unpackRowElem :: Get a -> Select (RowTag, a)
unpackRowElem unpack = rowElem >>= \ (idx, RowTagBytes bytes) -> pure (idx, runGet unpack bytes)

-- | Match the value of any type @a@ that instantiates 'IsPrimitive' with the current 'rowElem'
-- under the cursor.
readRowPrim :: forall a . IsPrimitive a => Select a
readRowPrim = do
  (idx, (RowTagBytes bytes)) <- rowElem
  case runGetOrFail (getPrimitive idx) bytes of
    Right (_, _, a) -> return a
    Left{}          -> mzero

----------------------------------------------------------------------------------------------------

type RowBuilder = RowBuilderM ()

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

-- | Use the 'IsPrimitive' instance for data of type @a@ to store the information in @a@ into a
-- 'RowBuilder'.
writeRowPrim :: forall a . IsPrimitive a => a -> RowBuilder
writeRowPrim a = RowBuilderM $ do
  idxelem <- lift $ putPrimitive a
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

instance DBMapped Char   where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Int    where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Int8   where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Int16  where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Int32  where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Int64  where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Word   where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Word8  where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Word16 where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Word32 where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Float  where { writeRow = writeRowPrim; readRow = readRowPrim; }
instance DBMapped Double where { writeRow = writeRowPrim; readRow = readRowPrim; }

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

----------------------------------------------------------------------------------------------------
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
    p = binPutVarWord35 . VarWord35

getTEST
  :: TEST Int64
  -> BLazy.ByteString
  -> Either TESTError
            (BLazy.ByteString, ByteOffset, TEST Int64)
getTEST orig bytes = execTest bytes $ liftBinGet Bin.get >>= \ (VarWord35 a) ->
  check (T1 a)     $ \ (VarWord35 b) -> 
  check (T2 a b)   $ \ (VarWord35 c) -> 
  check (T3 a b c) $ \ (VarWord35 d) -> done (T4 a b c d)
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

test_getPutVarWord35 :: IO ()
test_getPutVarWord35 = forM_ (loop (0::Int) p) $ \ (n, group) -> do
  print n
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
    p1 = 0 : 1 : (fromIntegral <$> Unboxed.toList all16BitPrimes)
    p2 = (\a ->   a * a  ) <$> p1
    p3 = (\a -> a * a * a) <$> p1
    p  = do
      (a, b, c) <- zip3 ((*) <$> p1 <*> p2) ((*) <$> p2 <*> p3) ((*) <$> p3 <*> p1)
      orig <- [T1 c, T2 a b, T3 a b c, T4 0 a b c, T4 a 0 b c, T4 a b 0 c, T4 a b c 0]
      let serialized = putTEST orig
      [(orig, serialized, getTEST orig $ BLazy.fromStrict serialized)]
    loop n p = seq n $! case splitAt (7*1024) p of
      ([],   []) -> []
      (group, p) -> (n, group) : loop (n + 7*1024) p
