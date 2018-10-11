{-# LANGUAGE TypeFamilies #-}

-- | A pretty printer oriented toward printing tables, sort of like spreadsheets.
module ProcGen.PrettyPrint
  ( PrettyPrintable(..),
    RowElem, Row, Doc(..), rowIndent, theRowIndent, rowElems, theRowElems,
    aggregA, aggreg,
    DocFolding, DocFolder, foldDocM, foldDoc,
    DocMapping, DocMapper, mapDocM, mapDocWithPauseM, mapDoc, mapDocWithPause,
    docMapperStep, docMapperRun, docMapperPause,
    Str.IsString(..),
  ) where

import           Control.Lens
import           Control.Monad.State

import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString             as Flat
import qualified Data.ByteString.Lazy        as Lazy
import qualified Data.ByteString.Lazy.UTF8   as UTF8
import           Data.Int
import qualified Data.Sequence               as S
import           Data.Semigroup
import qualified Data.String                 as Str
--import qualified Data.Text                   as Strict
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable
--import           Data.Word

----------------------------------------------------------------------------------------------------

-- | Create an aggregation function from an applicative fold function. The pure fold function will be
-- used to zip two rows. The first @row@ parameter is a default @row@ value to pass when one row is
-- longer than the other.
aggregA :: Applicative f => row -> (row -> row -> f fold) -> [row] -> [row] -> f [fold]
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

class PrettyPrintable a where
  pPrint :: a -> Doc

-- | A 'RowElem' is a continguous block of characters. This data type instantiates
-- 'Data.String.IsString', so if you make use of GHC's @OverloadedStrings@ language extension, you
-- can construct a 'RowElem' from a string literal in your code or from within GHCi. You can aso use
-- 'Data.String.fromString' to construct a 'RowElem' from a 'Prelude.String' type.
type RowElem = Unboxed.Vector Char
type IndentSpaces = Int

data Row
  = Row
    { theRowIndent :: !IndentSpaces
    , theRowElems  :: !Flat.ByteString
    }
  deriving (Eq, Ord)

instance Show Row where
  show Row{theRowIndent=ind,theRowElems=elems} = replicate ind ' ' ++
    unwords (Unboxed.toList <$> (unpackRowElems elems))

newtype Doc = Doc{ docToSequence :: S.Seq Row }
  deriving (Eq, Ord)

instance Semigroup Doc where { (Doc a) <> (Doc b) = Doc (a <> b); }
instance Monoid    Doc where { mempty = Doc mempty; mappend = (<>); }

-- | The amount of indentation
rowIndent :: Lens' Row Int
rowIndent = lens theRowIndent $ \ a b -> a{ theRowIndent = b }

-- | The elements in the row.
rowElems :: Lens' Row [RowElem]
rowElems = lens (unpackRowElems . theRowElems) $ \ a b -> a{ theRowElems = packRowElems b }

packRowElems :: [RowElem] -> Flat.ByteString
packRowElems = \ case
  []    -> Flat.empty
  elems -> Lazy.toStrict $ runPut $ do
    let objs = UTF8.fromString . Unboxed.toList <$> elems
    putWord32host $ fromIntegral $ length objs
    forM_ (zip objs elems) $ \ (obj, vec) -> do
      putWord32host $ fromIntegral $ Lazy.length    obj
      putWord32host $ fromIntegral $ Unboxed.length vec
    mapM_ putLazyByteString objs

unpackIndex :: Get [(Int64, Int)]
unpackIndex = do
  nelems <- fromIntegral <$> getWord32host
  replicateM nelems $ (,) <$> (fromIntegral <$> getWord32host) <*> (fromIntegral <$> getWord32host)

lazyBytesToRowElem :: Int -> Lazy.ByteString -> RowElem
lazyBytesToRowElem size bytes = Unboxed.create $ do
  vec <- Mutable.new size
  mapM_ (uncurry $ Mutable.write vec) $ zip [0 .. size-1] $ UTF8.toString bytes
  return vec

unpackRowElems :: Flat.ByteString -> [RowElem]
unpackRowElems = runGet getter . Lazy.fromStrict where
  getter = isEmpty >>= \ stop -> if stop then return [] else unpackIndex >>=
    mapM (\ (objLen, vecLen) -> lazyBytesToRowElem vecLen <$> getLazyByteString objLen)

----------------------------------------------------------------------------------------------------

class Monad m => DocFolding f fold m where
  toDocFold :: f -> Row -> DocFolder fold m ()

newtype DocFolder fold m a = DocFolder{ unwrapDocFolder :: StateT fold m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (DocFolder fold) where
  lift = DocFolder . lift

instance Monad m => MonadState fold (DocFolder fold m) where
  state = DocFolder . state

instance DocFolding (Row -> fold -> fold) fold Identity where
  toDocFold f row = modify $ f row

instance DocFolding (IndentSpaces -> [RowElem] -> fold -> fold) fold Identity where
  toDocFold f row = modify $ f (row ^. rowIndent) (row ^. rowElems)

instance DocFolding ([RowElem] -> fold -> fold) fold Identity where
  toDocFold f row = modify $ f (row ^. rowElems)

instance Monad m => DocFolding (Row -> fold -> m fold) fold m where
  toDocFold f row = get >>= lift . f row >>= put

instance Monad m => DocFolding (IndentSpaces -> [RowElem] -> fold -> m fold) fold m where
  toDocFold f row = get >>= lift . f (row ^. rowIndent) (row ^. rowElems) >>= put

instance Monad m => DocFolding ([RowElem] -> fold -> m fold) fold m where
  toDocFold f row = get >>= lift . f (row ^. rowElems) >>= put

foldDocM :: DocFolding f fold m => f -> fold -> Doc -> m fold
foldDocM f init (Doc doc) = execStateT (unwrapDocFolder $ mapM (toDocFold f) doc) init

foldDoc :: DocFolding f fold Identity => f -> fold -> Doc -> fold
foldDoc f init = runIdentity . foldDocM f init

----------------------------------------------------------------------------------------------------

class Monad m => DocMapping f m where
  toDocMap :: Monad m => f -> Row -> DocMapper m Row

data DocMapper m a
  = DocMapperPure a
  | DocMapperPause Row (Row -> DocMapper m a)
  | DocMapperLift (m (DocMapper m a))
  deriving Functor

instance Show (DocMapper m a) where
  show = \ case
    DocMapperPure{} -> "Done."
    DocMapperPause row _ -> "Paused on: "++show row
    DocMapperLift{} -> "Working..."

instance Monad m => Monad (DocMapper m) where
  return = DocMapperPure
  f >>= next = case f of
    DocMapperPure      a -> next a
    DocMapperPause row f -> DocMapperPause row $ (>>= next) <$> f
    DocMapperLift      f -> DocMapperLift $ (>>= next) <$> f

instance Applicative m => Applicative (DocMapper m) where
  pure = DocMapperPure
  f <*> a = case f of
    DocMapperPure      f -> f <$> a
    DocMapperPause row f -> DocMapperPause row $ (<*> a) <$> f
    DocMapperLift      f -> DocMapperLift $ (<*> a) <$> f

docMapperPause :: Monad m => Row -> DocMapper m Row
docMapperPause row = DocMapperPause row return

-- | Check if a 'DocMapper' was paused, if so evaluate the given continuation. Otherwise evaluate
-- the next step and pause again.
docMapperStep :: Monad m => DocMapper m a -> (Row -> m Row) -> m (DocMapper m a)
docMapperStep f onPause = case f of
  DocMapperPure      a -> return (DocMapperPure a)
  DocMapperPause row f -> f <$> onPause row
  DocMapperLift      a -> a

-- | Loop until completion, applying the given 'Row' function whenever a pause occurs.
docMapperRun :: Monad m => DocMapper m a -> (Row -> m Row) -> m a
docMapperRun f onPause = case f of
  DocMapperPure      a -> return a
  DocMapperPause row f -> onPause row >>= flip docMapperRun onPause . f
  DocMapperLift      f -> f >>= flip docMapperRun onPause

-- | Map over a 'Doc' with a given mapping function, and also takes a pause function to be evaluated
-- whenever the mapping pauses.
mapDocWithPauseM :: DocMapping f m => (Row -> m Row) -> f -> Doc -> m Doc
mapDocWithPauseM onPause f (Doc doc) = loop S.empty doc where
  loop back = \ case
    S.Empty      -> return $ Doc back
    a S.:<| more -> docMapperRun (toDocMap f a) onPause >>= flip loop more . (back S.|>)

-- | Like 'mapDocWithPauseM' but automatically resumes after every pause.
mapDocM :: DocMapping f m => f -> Doc -> m Doc
mapDocM = mapDocWithPauseM pure

mapDocWithPause :: DocMapping f Identity => (Row -> Row) -> f -> Doc -> Doc
mapDocWithPause onPause f = runIdentity . mapDocWithPauseM (pure . onPause) f

mapDoc :: DocMapping f Identity => f -> Doc -> Doc
mapDoc = mapDocWithPause id
