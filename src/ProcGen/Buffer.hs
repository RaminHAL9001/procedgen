-- | A module for working with mutable buffers of unboxed types, most typically buffers of
-- 'ProcGen.Types.Sample's.a
module ProcGen.Buffer
  ( module ProcGen.Buffer
  , module Control.Monad.Primitive
  , module Control.Monad.State
  ) where

import           ProcGen.Types

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.State

import qualified Data.Vector.Unboxed.Mutable as Mutable

----------------------------------------------------------------------------------------------------

-- | Find the minimum and maximum element in a mutable 'Mutable.MVector', evaluates to an error if
-- the vector is empty so this function is not total.
minMaxBuffer
  :: forall m elem . (Mutable.Unbox elem, Ord elem, PrimMonad m)
  => Mutable.MVector (PrimState m) elem -> m (MinMax elem)
minMaxBuffer vec = do
  if Mutable.length vec == 0 then error $ "minMaxVec called on empty vector" else do
    init <- Mutable.read vec 0
    foldBuffer vec (TimeWindow 0 $ Mutable.length vec) (MinMax init init) $
      const $ modify . flip stepMinMax

-- | Not to be confused with the Gaussian 'ProcGen.Types.normal' function. Given a minimum and
-- maximum value, this function performs a simple linear transformation that normalizes all elements
-- in the given mutable 'Mutable.MVector' buffer.
normalizeBuffer
  :: (Eq elem, Ord elem, Num elem, Fractional elem, Mutable.Unbox elem, PrimMonad m)
  => Mutable.MVector (PrimState m) elem -> MinMax elem -> m ()
normalizeBuffer vec (MinMax lo hi) = do
  let offset = (hi + lo) / 2
  let scale  = (hi - lo) / 2
  unless (scale == 0) $
    mapBuffer vec (TimeWindow 0 $ Mutable.length vec) $ const $ pure . (/ scale) . subtract offset

----------------------------------------------------------------------------------------------------

-- | Apply a function to all elements in the buffer along the indicies given by the
-- 'ProcGen.Types.TimeWindow' with a function over each index. The elements or only read, no
-- elements are written to the buffer.
foldBuffer
  :: (Mutable.Unbox elem, PrimMonad m)
  => Mutable.MVector (PrimState m) elem
  -> TimeWindow Int
  -> fold
  -> (Int -> elem -> StateT fold m ())
  -> m fold
foldBuffer buf win fold f = flip execStateT fold $ forM_ (twEnum win) $ \ i ->
  lift (Mutable.read buf i) >>= f i

-- | Update a sequence of elements in a buffer along the indicies given by the
-- 'ProcGen.Types.TimeWindow' with a function over each index and element.
mapBuffer
  :: (Mutable.Unbox elem, PrimMonad m)
  => Mutable.MVector (PrimState m) elem
  -> TimeWindow Int
  -> (Int -> elem -> m elem)
  -> m ()
mapBuffer buf win f = foldMapBuffer buf win () $ \ i -> lift . f i

-- | Like 'mapBuffer' but never reads elements from the buffer before applying the function. The
-- function is applied to all indicies given by the 'ProcGen.Types.TimeWindow' and each result is
-- written to the element in the buffer at each index.
writeBuffer
  :: (Mutable.Unbox elem, PrimMonad m)
  => Mutable.MVector (PrimState m) elem
  -> TimeWindow Int
  -> (Int -> m elem)
  -> m ()
writeBuffer buf win f = foldWriteBuffer buf win () (lift . f)

-- | Apply a function to all elements in the buffer along indicies given by the
-- 'ProcGen.Types.TimeWindow' with a function over each index. The elements are not read, only
-- written.
foldWriteBuffer
  :: (Mutable.Unbox elem, PrimMonad m)
  => Mutable.MVector (PrimState m) elem
  -> TimeWindow Int
  -> fold
  -> (Int -> StateT fold m elem)
  -> m fold
foldWriteBuffer buf win fold f = flip execStateT fold $ forM_ (twEnum win) $ \ i ->
  f i >>= lift . Mutable.write buf i

-- | Apply a function to all elements in the buffer along indicies given by the
-- 'ProcGen.Types.TimeWindow' with a function over each index-element association. A state monad
-- transformer function is used as the function to read and apply elements, that way you can perform
-- some stateful tracking statistics on the elements applied.
foldMapBuffer
  :: (Mutable.Unbox elem, PrimMonad m)
  => Mutable.MVector (PrimState m) elem
  -> TimeWindow Int
  -> fold -- ^ a value to fold as functions are applied
  -> (Int -> elem -> StateT fold m elem)
  -> m fold
foldMapBuffer buf win fold f = flip execStateT fold $ forM_ (twEnum win) $ \ i ->
  lift (Mutable.read buf i) >>= f i >>= lift . Mutable.write buf i

----------------------------------------------------------------------------------------------------

-- | Sometimes if you want to extract the length of a vector, you will find that you cannot use the
-- @let@ statement to do it, for example:
--
-- @
-- myCopyBuffer :: ('Mutable.Unbox' elem, 'Control.Monad.Primitive.PrimMonad' m) => 'Mutable.MVector' ('Control.Monad.Primitive.PrimState m) elem -> m ()
-- myCopyBuffer buffer = do
--     let len = 'Mutable.length' buffer -- TYPE ERROR!!!
--     newBuffer <- 'Mutable.new' len
--     'Control.Monad.forM_' [0 .. len-1] (\\ idx -> 'Mutable.read' buffer idx >>= 'Mutable.write' newBuffer idx)
-- @
--
-- A type error will occur on the @let@ statement because @let@ is desigend to basically lock a type
-- down so that it can be memoized. But if you lock-down a @forall@-ed type you are basically saying
-- that the type of function used in the @let@ statement __MUST__ be different from the type used in
-- the 'Mutable.read' and 'Mutable.write' statements, and therefore this function will fail to
-- type check.
--
-- You usually solve the problem by not using @let@ at all, instead using the @where@ keyword, or
-- else simply writing out the full expression: @('Mutable.length' buffer)@ everywhere it is
-- used. However if you really need to have the buffer length as a point symbol in your code, you
-- can use this 'bufferLength' function to do so. The type @m@ on the @'Mutable.MVector
-- ('Control.Monad.Primitive.PrimState' m)@ value matches with the return value @m Int@ so the type
-- checker can prove that your mutable vector is the same type as all the others used in your
-- function.
bufferLength :: (Mutable.Unbox elem, PrimMonad m) => Mutable.MVector (PrimState m) elem -> m Int
bufferLength = pure . Mutable.length
