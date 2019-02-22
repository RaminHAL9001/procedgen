{-# LANGUAGE NoMonomorphismRestriction #-}
-- | A typeclass providing a consistent interface to various objects which can be marshalled to an
-- immutable unboxed Vector representation. Objects of this type should be stored in memory as
-- vectors and should be observed by being converted to lazy data types.
module ProcGen.VectorBuilder where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Primitive
import           Control.Monad.State.Strict
import           Control.Monad.ST

import           Data.Semigroup
import qualified Data.Vector.Generic.Mutable.Base  as GMVec
import qualified Data.Vector.Generic.Mutable       as GMVec
import qualified Data.Vector.Unboxed               as UVec

----------------------------------------------------------------------------------------------------

-- | A function type for buliding mutable 'UVec.Vector's with a function that uses a cursor to move
-- around withtin a mutable 'Vec.Vector'.
newtype VectorBuilder mvec elem m a
  = VectorBuilder
    { unwrapVectorBuilder :: StateT (VectorBuilderState mvec elem m) m a
    }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (VectorBuilder mvec elem) where
  lift = VectorBuilder . lift

instance Monad m => MonadState (VectorBuilderState mvec elem m) (VectorBuilder mvec elem m) where
  state = VectorBuilder . state

instance (Monad m, Semigroup a) => Semigroup (VectorBuilder mvec elem m a) where
  a <> b = liftM2 (<>) a b

instance (Monad m, Monoid a) => Monoid (VectorBuilder mvec elem m a) where
  mempty = return mempty
  mappend a b = liftM2 mappend a b

data VectorBuilderState mvec elem m
  = VectorBuilderState
    { theBuilderCursor :: !Int
    , theBuilderVector :: !(mvec (PrimState m) elem)
    }

-- | Given an initial vector size, allocate an 'GMVec.STVector' of that size and then evaluate a
-- 'VectorBuilder' function to populate the 'GMVec.STVector'. When 'VectorBuilder' function
-- evaluation completes, freeze the 'GMVec.STVector' to a 'UVec.Vector' and eliminate the monad
-- entirely, becoming a pure function.
runVectorBuilderST
  :: (UVec.Unbox elem)
  => Int -> VectorBuilder UVec.MVector elem (ST s) a -> ST s (a, UVec.MVector s elem)
runVectorBuilderST size build = do
  vec <- GMVec.new size
  liftM (fmap theBuilderVector) $ runStateT (unwrapVectorBuilder build) VectorBuilderState
    { theBuilderCursor = 0
    , theBuilderVector = vec
    }

-- | Evaluate 'runVectorBuilderST' and then immediately dissolve the 'Control.Monad.ST.ST' monad and
-- freeze the mutable 'UVec.MVector' into a pure, immutable 'UVec.Vector'.
runVectorBuilder
  :: (UVec.Unbox elem)
  => Int -> (forall s . VectorBuilder UVec.MVector elem (ST s) void) -> UVec.Vector elem
runVectorBuilder size build = UVec.create $ liftM snd $ runVectorBuilderST size build

-- | A lens to access the cursor at which 'builderPutElem' and 'builderGetElem' will put or get an
-- element value.
builderCursor
  :: (PrimMonad m, GMVec.MVector vec elem)
  => Lens' (VectorBuilderState vec elem m) Int
builderCursor = lens theBuilderCursor $ \ a b -> a{ theBuilderCursor = b }

-- | A lens to access the vector that is currently being filled with @elem@ values.
builderVector
  :: (PrimMonad m, GMVec.MVector vec elem)
  => Lens' (VectorBuilderState vec elem m) (vec (PrimState m) elem)
builderVector = lens theBuilderVector $ \ a b -> a{ theBuilderVector = b }

-- | Take the value of an @elem@ at the current 'builderCursor' in the 'builderVector'.
builderGetElem :: (PrimMonad m, GMVec.MVector vec elem) => VectorBuilder vec elem m elem
builderGetElem = GMVec.read <$> use builderVector <*> use builderCursor >>= lift

-- | Reposition the 'builderCursor'. Returns the value of the 'builderCursor' as it was set before the
-- @('Int' -> 'Int')@ function was applied.
modifyCursor :: (PrimMonad m, GMVec.MVector vec elem) => (Int -> Int) -> VectorBuilder vec elem m Int
modifyCursor = (<*) (use builderCursor) . (builderCursor %=)

-- | Get the current length of the vector allocation size being built.
maxBuildLength :: (PrimMonad m, GMVec.MVector vec elem) => VectorBuilder vec elem m Int
maxBuildLength = GMVec.length <$> use builderVector

-- | Force re-allocation of the vector using 'GMVec.grow' or 'GMVec.take', depending on whether the
-- updated size value computed by the given @('Int' -> 'Int')@ function is greater or less than the
-- current vector size.
resizeVector
  :: (PrimMonad m, GMVec.MVector vec elem)
  => (Int -> Int)
  -> VectorBuilder vec elem m ()
resizeVector f = do
  oldsize <- maxBuildLength
  let newsize = f oldsize
  if oldsize == newsize then return () else
    if oldsize < newsize
     then GMVec.unsafeGrow <$> use builderVector <*> pure newsize >>= lift >>= (builderVector .=)
     else GMVec.take newsize <$> use builderVector >>= (builderVector .=)

-- | Useful when defining a 'ProcGen.Collapsible.collapse' function, this function takes a record
-- field for a @data@ type and the data value itself, and accesses the element from the data value
-- using the record and passes the record value to 'buildStep'. This allows you to define
-- 'ProcGen.Collapsible.collapse' functions like so:
--
-- @
-- data Point2D = Point2D{ x :: !'Prelude.Double', y :: !'Prelude.Double' }
-- instance 'ProcGen.Collapsible.Collapsible' 'Prelude.Double' Point2D where
--     'ProcGen.Collapsible.collapse' = 'buildRecord' x 'Data.Semigroup.<>' 'buildRecord' y
-- @
buildRecord
  :: (PrimMonad m, GMVec.MVector vec elem)
  => (a -> elem) -> a -> VectorBuilder vec elem m ()
buildRecord record dat = buildStep (record dat)

-- | Place an @elem@ value at the current 'builderCursor', and then increment the 'builderCursor'.
buildStep :: (PrimMonad m, GMVec.MVector vec elem) => elem -> VectorBuilder vec elem m ()
buildStep e = do
  GMVec.write <$> use builderVector <*> use builderCursor <*> pure e >>= lift
  builderCursor += 1

-- | Get the @elem@ value at the current 'builderCursor', and then increment the 'builderCursor'.
buildTake1 :: (PrimMonad m, GMVec.MVector vec elem) => VectorBuilder vec elem m elem
buildTake1 =
  (GMVec.read <$> use builderVector <*> use builderCursor >>= lift) <*
  (builderCursor += 1)

-- | The 'buildTake1' value applied 2 times to return a double, and thus advancing the
-- 'builderCursor' by 2 indicies as well.
buildTake2 :: (PrimMonad m, GMVec.MVector vec elem) => VectorBuilder vec elem m (elem, elem)
buildTake2 = (,) <$> buildTake1 <*> buildTake1

-- | The 'buildTake1' value applied 3 times to return a tripple, and thus advancing the
-- 'builderCursor' by 3 indicies as well.
buildTake3 :: (PrimMonad m, GMVec.MVector vec elem) => VectorBuilder vec elem m (elem, elem, elem)
buildTake3 = (,,) <$> buildTake1 <*> buildTake1 <*> buildTake1

-- | The 'buildTake1' value applied 4 times to return a tripple, and thus advancing the
-- 'builderCursor' by 4 indicies as well.
buildTake4
  :: (PrimMonad m, GMVec.MVector vec elem)
  => VectorBuilder vec elem m (elem, elem, elem, elem)
buildTake4 = (,,,) <$> buildTake1 <*> buildTake1 <*> buildTake1 <*> buildTake1

-- | The 'buildTake1' value applied 5 times to return a tripple, and thus advancing the
-- 'builderCursor' by 5 indicies as well.
buildTake5
  :: (PrimMonad m, GMVec.MVector vec elem)
  => VectorBuilder vec elem m (elem, elem, elem, elem, elem)
buildTake5 = (,,,,) <$> buildTake1 <*> buildTake1 <*> buildTake1 <*> buildTake1 <*> buildTake1

-- | The 'buildTake1' value applied 5 times to return a tripple, and thus advancing the
-- 'builderCursor' by 5 indicies as well.
buildTake6
  :: (PrimMonad m, GMVec.MVector vec elem)
  => VectorBuilder vec elem m (elem, elem, elem, elem, elem, elem)
buildTake6 = (,,,,,) <$> buildTake1 <*> buildTake1 <*>
  buildTake1 <*> buildTake1 <*> buildTake1 <*> buildTake1

-- | A range building function. This function is required as a parameter by 'buildMapRange',
-- 'buildMapRangeM', 'buildScanRange', 'buildScanRangeM', 'buildFoldRange', and
-- 'buildFoldRangeM'. Functions which take a range constructor pass two values: the cursor,
-- and the length of the array. You then return a tuple with the range you want to scan over.
type MakeRange = Int -> Int -> (Int, Int)

-- | A 'MakeRange' function that ranges the entire 'builderVector'.
wholeVector :: MakeRange
wholeVector _ len = rangeFence 0 len

-- | A 'MakeRange' function that ranges all elements in the 'builderVector' up to but not including
-- the current 'builderCursor'.
allBeforeCursor :: MakeRange
allBeforeCursor i _ = rangeFence 0 i

-- | A 'MakeRange' function that ranges all elements from the current 'builderCursor' (including the
-- element under the cursor) all the way to the final element in the 'GMVec.Vector'.
allAfterCursor :: MakeRange
allAfterCursor = rangeFence

-- | Takes a start and end index value and constructs a range that includes the start index value,
-- and runs up until but does not include, the end index value. The word "fence" here refers to
-- "fencepost bugs" in which a programmer accesses an array in which indicies begin at zero, and the
-- index accessed is equal to the length of the array which is beyond the final fencepost. Use this
-- function if you are coding an algorithm in which you might make make that mistake.
rangeFence :: MakeRange
rangeFence a b = (a, b-1)

-- | Map elements over a range, updating them as you go.
buildMapRange
  :: (PrimMonad m, GMVec.MVector vec elem)
  => MakeRange
  -> (Int -> elem -> elem)
  -> VectorBuilder vec elem m ()
buildMapRange makeRange f = buildMapRangeM makeRange (\ i -> pure . f i)

-- | A version of 'buildMapRange' that takes a monadic mapping function.
buildMapRangeM
  :: (PrimMonad m, GMVec.MVector vec elem)
  => MakeRange
  -> (Int -> elem -> VectorBuilder vec elem m elem)
  -> VectorBuilder vec elem m ()
buildMapRangeM makeRange f = do
  vec   <- use builderVector
  range <- makeRange <$> use builderCursor <*> maxBuildLength
  forM_ range $ \ i -> lift (GMVec.read vec i) >>= f i >>= lift . GMVec.write vec i

-- | Scan over a range of @elem@ values, updating a stateful value with each @elem@, and returning
-- an optional new value to write back to the 'builderVector'.
buildScanRange
  :: (PrimMonad m, GMVec.MVector vec elem)
  => MakeRange -> st
  -> (Int -> elem -> st -> (Maybe elem, st))
  -> VectorBuilder vec elem m st
buildScanRange makeRange st f = buildScanRangeM makeRange st (\ i e -> pure . f i e)

-- | A version of 'buildScanRange' that takes a monadic scanning function.
buildScanRangeM
  :: (PrimMonad m, GMVec.MVector vec elem)
  => MakeRange -> st
  -> (Int -> elem -> st -> VectorBuilder vec elem m (Maybe elem, st))
  -> VectorBuilder vec elem m st
buildScanRangeM makeRange st f = do
  vec <- use builderVector
  makeRange <$> use builderCursor <*> maxBuildLength >>=
    foldM (\ st i -> lift (GMVec.read vec i) >>= flip (f i) st >>= \ (update, st) ->
      maybe (return ()) (lift . GMVec.write vec i) update >> return st) st

-- | Like 'buildScanRangeM' but does not update any elements.
buildFoldRangeM
  :: (PrimMonad m, GMVec.MVector vec elem)
  => MakeRange -> st
  -> (Int -> elem -> st -> VectorBuilder vec elem m st)
  -> VectorBuilder vec elem m st
buildFoldRangeM makeRange st f = do
  vec <- use builderVector
  makeRange <$> use builderCursor <*> maxBuildLength >>=
    foldM (\ st i -> lift (GMVec.read vec i) >>= flip (f i) st) st

----------------------------------------------------------------------------------------------------


