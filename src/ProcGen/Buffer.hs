-- | A module for working with mutable buffers of unboxed types, most typically buffers of
-- 'ProcGen.Types.Sample's.a
module ProcGen.Buffer
  ( module ProcGen.Buffer
  , module Control.Monad.Primitive
  ) where

import           ProcGen.Types

import           Control.Monad
import           Control.Monad.Primitive

import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed.Mutable as Mutable

----------------------------------------------------------------------------------------------------

-- | Create a new buffer large enough to store 'ProcGen.Types.Duration' seconds worth of
-- 'ProcGen.Types.Sample's. Note that the return type of this function is polymorphic and can be
-- unified with either a type of 'Mutable.STVector' or an 'Mutable.IOVector'.
newBuffer :: PrimMonad m => Duration -> m (Mutable.MVector (PrimState m) Sample)
newBuffer = Mutable.new . (+ 1) . durationSampleCount

-- | Find the minimum and maximum element in a mutable 'Mutable.MVector', evaluates to an error if
-- the vector is empty so this function is not total.
minMaxVec
  :: forall m elem . (Mutable.Unbox elem, Ord elem, PrimMonad m)
  => Mutable.MVector (PrimState m) elem -> m (elem, elem)
minMaxVec vec = do
  if Mutable.length vec == 0 then error $ "minMaxVec called on empty vector" else do
    init <- Mutable.read vec 0
    lo <- newMutVar init
    hi <- newMutVar init
    forM_ [1 .. Mutable.length vec - 1] $ \ i ->
      Mutable.read vec i >>= \ elem -> modifyMutVar lo (min elem) >> modifyMutVar hi (max elem)
    liftM2 (,) (readMutVar lo) (readMutVar hi)

-- | Not to be confused with the Gaussian 'normal' function. Given a minimum and maximum value, this
-- function performs a simple linear transformation that normalizes all elements in the given
-- 'Mutable.STVector'.
normalize
  :: (Eq elem, Num elem, Fractional elem, Mutable.Unbox elem, PrimMonad m)
  => Mutable.MVector (PrimState m) elem -> (elem, elem) -> m ()
normalize vec (lo, hi) = do
  let offset = (hi + lo) / 2
  let scale  = (hi - lo) / 2
  unless (scale == 0) $ forM_ [0 .. Mutable.length vec - 1] $ \ i ->
    Mutable.read vec i >>= Mutable.write vec i . (/ scale) . subtract offset

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
