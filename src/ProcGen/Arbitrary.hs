-- | A class of things that can be procedurally generated from random numbers generated by the
-- Twofish random number generator. The "Control.Monad.Random.Class" monad is re-exported, providing
-- the 'Control.Monad.Random.Class.getRandomR', 'Control.Monad.Random.Class.getRandom',
-- 'Control.Monad.Random.Class.getRandomRs', and 'Control.Monad.Random.Class.getRandoms' functions
-- which you can use to define instances of the 'arbitrary' function for your procedurally generated
-- data types.
module ProcGen.Arbitrary
  ( Arbitrary(..), onArbitrary,
    onRandFloat, onBiasedRandFloat, onBeta5RandFloat, onNormalRandFloat, floatToIntRange,
    Word256, TFRandSeed, tfGen,
    TFRandT(..), TFRand,  arbTFRand, seedIOArbTFRand,
    seedEvalTFRand, seedEvalTFRandT, seedIOEvalTFRand, seedIOEvalTFRandT, evalTFRand, evalTFRandT,
     seedRunTFRand,  seedRunTFRandT,  seedIORunTFRand,  seedIORunTFRandT, runTFRand,  runTFRandT,
    testDistributionFunction,
    System.Random.TF.Init.initTFGen,
    System.Random.TF.TFGen,
    module Control.Monad.Random.Class,
  ) where

import           ProcGen.Types

import           Control.Arrow
import           Control.Exception (evaluate)
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans
import           Control.Monad.Random.Class
import           Control.Monad.Trans.Random.Lazy

import           Data.Bits
import           Data.Ratio ((%))
import           Data.Semigroup
import           Data.Functor.Identity
import qualified Data.Vector.Unboxed              as Unboxed
import qualified Data.Vector.Unboxed.Mutable      as Mutable
import           Data.Word

import           System.Random.TF
import           System.Random.TF.Init

----------------------------------------------------------------------------------------------------

-- | This is a function which defines a common interface for procedurally generating values from an
-- arbitrary random number generator, where the random number generator is defined to be an instance
-- of the 'Control.Monad.Random.Class.MonadRandom' class. Minimal complete definition is just
-- the 'arbitrary' function.
class Arbitrary a where
  -- | Produce a single random value in a function type @m@ which must be a member of the
  -- 'Control.Monad.Random.Class.MonadRandom' class.
  arbitrary :: (Functor m, Monad m, Applicative m, MonadRandom m) => m a
  -- | Generates a list containing between @a@ and @b@ elements, where the interval
  -- @(a::'Prelude.Int',b::'Prelude.Int')@ is chosen from a uniform distribution (all numbers
  -- equally probable).
  --
  -- This function is a member of the 'Arbitrary' class, so you can provide your own list-generating
  -- version of 'arbtirary' to provide bias to the given range.
  arbitraryList :: (Functor m, Monad m, Applicative m, MonadRandom m) => (Int, Int) -> m [a]
  arbitraryList = getRandomR >=> flip replicateM arbitrary

-- | This function can be passed as a continuation to one of the 'onRandFloat' family of functions
-- to transform a 'ProcGenFloat' to an integral value. The two parameters to this function are the
-- lower bound (the result will be greater or equal to this value), and the upper bound (the result
-- will be less than and NOT equal to this value).
floatToIntRange :: Integral i => i -> i -> ProcGenFloat -> i
floatToIntRange lo hi x = let lof = realToFrac lo in floor $ (realToFrac hi - lof) * x + lof

-- | Often you obtain a random value and then immediately perform some transformation on it by using
-- 'fmap' or 'Control.Monad.liftM'. This function allows you to specify the transformation as a
-- parameter without using 'fmap' or 'Control.Monad.liftM'.
onArbitrary :: (MonadRandom m, Arbitrary a) => (a -> b) -> m b
onArbitrary f = f <$> arbitrary

-- | Evaluate a continuation with random floating point value between 0 and 1, the random number
-- begin generated by an unbiased uniform probability distribution.
onRandFloat :: MonadRandom m => (ProcGenFloat -> a) -> m a
onRandFloat f = f <$> getRandom

-- | Evaluate 'onRandFloat' but apply a bias defined by a 'CumulativeDistributionTable' then pass
-- the biased random number to the continuation.
onBiasedRandFloat :: MonadRandom m => CumulativeDistributionTable -> (ProcGenFloat -> a) -> m a
onBiasedRandFloat table f = f . inverseTransformSample table <$> getRandom

-- | Calls 'onBiasedRandFloat' with the 'inverseBeta5Table' as the 'CumulativeDistributionFunction',
-- so the random number produced will be between 0 and 1 and most probably near @1/5@.
onBeta5RandFloat :: MonadRandom m => (ProcGenFloat -> b) -> m b
onBeta5RandFloat = onBiasedRandFloat inverseBeta5Table

-- | Calls 'onBiasedRandFloat' with the 'inverseNormalTable' as the
-- 'CumulativeDistributionFunction', producing a number between 0 and 1, but most probably near
-- @1/2@.
onNormalRandFloat :: MonadRandom m => (ProcGenFloat -> b) -> m b
onNormalRandFloat = onBiasedRandFloat inverseNormalTable

----------------------------------------------------------------------------------------------------

-- | A twofish random number seed stored as a single data type.
type TFRandSeed = Word256

data Word256 = Word256 !Word64 !Word64 !Word64 !Word64
  deriving (Eq, Ord)

instance Num Word256 where
  (+) a b = listToWord256 $ snd $ foldl
    (\ (prevCarry, stk) (carry, a) -> (carry, (if prevCarry == 0 then 0 else a+1) : stk))
    (0, [])
    (uncurry word64AddWithCarry <$> zip (word256toList a) (word256toList b))
  (*) a b = sum $ do
    let toDigits (Word256 s3 s2 s1 s0) =
          [ (s3, \ (_ , s3) -> Word256 s3  0  0  0)
          , (s2, \ (s3, s2) -> Word256 s3 s2  0  0)
          , (s1, \ (s2, s1) -> Word256  0 s2 s1  0)
          , (s0, \ (s1, s0) -> Word256  0  0 s1 s0)
          ]
    ((a, _), (b, constr)) <- (,) <$> toDigits a <*> toDigits b
    [constr $ word64MultWithCarry a b]
  negate = \ case
    (Word256  0  0  0  0) -> zeroBits
    (Word256 s3 s2 s1 s0) -> 1 +
      Word256 (complement s3) (complement s2) (complement s1) (complement s0)
  abs = id
  signum = \ case { (Word256 0 0 0 0) -> 0; _ -> 1; }
  fromInteger =
    let divisor = 1 + toInteger (maxBound :: Word64)
        loop n_0 = let (n_1, remainder) = divMod n_0 divisor in fromInteger remainder : loop n_1
    in  listToWord256 . loop

instance Enum Word256 where
  succ = (+ 1)
  pred = subtract 1
  toEnum = fromIntegral
  fromEnum = fromIntegral
  enumFrom = iterate succ
  enumFromThen a b  = iterate (+ (b - a)) a

instance Real Word256 where
  toRational a = toInteger a % 1

instance Integral Word256 where
  quotRem a b = fromInteger *** fromInteger $ quotRem (toInteger a) (toInteger b)
  toInteger (Word256 s3 s2 s1 s0) = let b = 2^(64::Int) in
    ((toInteger s3 * b + toInteger s2) * b + toInteger s1) * b + toInteger s0

instance Bounded Word256 where
  minBound = zeroBits
  maxBound = Word256 maxBound maxBound maxBound maxBound

instance Bits Word256 where
  isSigned     = const False
  bitSize      = const 256
  bitSizeMaybe = const (Just 256)
  popCount (Word256 s3 s2 s1 s0) =
    let f off s next = let n = popCount s in if n > 0 then n + off else next
    in  f 192 s3 $ f 128 s2 $ f 64 s1 $ popCount s0
  zeroBits = Word256 0 0 0 0
  (.|.) = word256BinOp (.|.)
  (.&.) = word256BinOp (.|.)
  xor   = word256BinOp xor
  complement = word256FMap complement
  rotate        w i = listToWord256 $ word64ShiftList (mod i 256) $ cycle $ word256toList w
  shift         w i = if abs i >= 256 then zeroBits else
    listToWord256 $ word64ShiftList i $ word256toList w
  bit             = word256Bit zeroBits        setBit zeroBits
  setBit        w = word256Bit        w        setBit        w
  clearBit      w = word256Bit        w      clearBit        w
  complementBit w = word256Bit        w complementBit        w
  testBit (Word256 s3 s2 s1 s0) i = case div i 64 of
    0 -> testBit s0 i
    1 -> testBit s1 i
    2 -> testBit s2 i
    3 -> testBit s3 i
    _ -> zeroBits

word256FMap :: (Word64 -> Word64) -> Word256 -> Word256
word256FMap f (Word256 s3 s2 s1 s0) = Word256 (f s3) (f s2) (f s1) (f s0)

word256BinOp :: (Word64 -> Word64 -> Word64) -> Word256 -> Word256 -> Word256
word256BinOp f (Word256 a3 a2 a1 a0) (Word256 b3 b2 b1 b0) =
  Word256 (f a3 b3) (f a2 b2) (f a1 b1) (f a0 b0)

word256Bit :: Word256 -> (Word64 -> Int -> Word64) -> Word256 -> Int -> Word256
word256Bit deflt f (Word256 s3 s2 s1 s0) = \ case
  i | i <  64 -> Word256 s3 s2 s1 (f s0 $ i -   0)
  i | i < 128 -> Word256 s3 s2 (f s1 $ i -  64) s0
  i | i < 192 -> Word256 s3 (f s2 $ i - 128) s1 s0
  i | i < 256 -> Word256 (f s3 $ i - 192) s2 s1 s0
  _           -> deflt

word256toList :: Word256 -> [Word64]
word256toList (Word256 s3 s2 s1 s0) = [s0, s1, s2, s3]

listToWord256 :: [Word64] -> Word256
listToWord256 = \ case
  [] -> zeroBits
  [s3] -> Word256 0 0 0 s3
  [s3,s2] -> Word256 0 0 s2 s3
  [s3,s2,s1] -> Word256 0 s1 s2 s3
  s3:s2:s1:s0:_ -> Word256 s0 s1 s2 s3

word64ShiftList :: Int -> [Word64] -> [Word64]
word64ShiftList i = \ case
  []   -> []
  a:ax ->
    let (d, r) = divMod i 64
        loop prev (next, a) = \ case
          []   -> [a .|. prev, next]
          b:ax -> (a .|. prev) : loop next (word64ShiftWithCarry b r) ax
    in (if d < 0 then drop (abs d) else ((replicate d 0) ++)) $ loop 0 (word64ShiftWithCarry a r) ax

word64ShiftWithCarry :: Word64 -> Int -> (Word64, Word64)
word64ShiftWithCarry w i = if i == 0 then (0, w) else
  if i < 0 then (shift w i, shift w (64 + i)) else (shift w (64 - i), shift w i)

word64AddWithCarry :: Word64 -> Word64 -> (Word64, Word64)
word64AddWithCarry a b = let top = (/= 0) . (.&.) (shift 1 63) in
  (if top a && top b then 1 else 0, a + b)

word64SumWithCarry :: [(Word64, Word64)] -> (Word64, Word64)
word64SumWithCarry = foldl
  (\ (accHI, accLO_0) (hi, lo) ->
     let (carry, accLO) = word64AddWithCarry accLO_0 lo
     in (accHI + hi + carry, accLO)
  ) (0, 0)

word64MultWithCarry :: Word64 -> Word64 -> (Word64, Word64)
word64MultWithCarry a b = word64SumWithCarry
  [(shift a $ 64 - i, shift a i) | i <- [0 .. 63], testBit b i]

-- | Construct a 'TFGen' from a 'TFGenSeed'
tfGen :: TFRandSeed -> TFGen
tfGen (Word256 s3 s2 s1 s0) = seedTFGen (s3, s2, s1, s0)

----------------------------------------------------------------------------------------------------

-- | A simple default pure random number generator based on the Twofish pseudo-random number
-- generator provided by the 'System.Random.TF.Gen'. This function type instantiates the
-- 'Control.Monad.Random.Class.MonadRandom' class so that you can use this to evaluate an instance
-- of 'arbitrary'. 
newtype TFRandT m a = TFRandT { unwrapTFRandT :: RandT TFGen m a }
  deriving (Functor, Applicative, Monad, MonadIO)

type TFRand a = TFRandT Identity a

instance Monad m => MonadRandom (TFRandT m) where
  getRandomR  = TFRandT . getRandomR
  getRandom   = TFRandT getRandom
  getRandomRs = TFRandT . getRandomRs
  getRandoms  = TFRandT getRandoms

instance MonadTrans TFRandT where { lift = TFRandT . lift; }

instance (Monad m, Semigroup a) => Semigroup (TFRandT m a) where
  a <> b = (<>) <$> a <*> b

instance (Monad m, Monoid a) => Monoid (TFRandT m a) where
  mempty      = pure mempty
  mappend a b = mappend <$> a <*> b

instance (Num n, Monad m) => Num (TFRandT m n) where
  (+) a b     = (+) <$> a <*> b
  (-) a b     = (-) <$> a <*> b
  (*) a b     = (*) <$> a <*> b
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

instance (Fractional n, Monad m) => Fractional (TFRandT m n) where
  (/) a b      = (/) <$> a <*> b
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Floating n, Monad m) => Floating (TFRandT m n) where
  { pi = pure pi; exp = fmap exp; log = fmap log; sqrt = fmap sqrt;
    (**) a b = (**) <$> a <*> b; logBase a b = logBase <$> a <*> b;
    sin   = fmap sin;   cos   = fmap cos;   tan   = fmap tan;
    asin  = fmap asin;  acos  = fmap acos;  atan  = fmap atan;
    sinh  = fmap sinh;  cosh  = fmap cosh;  tanh  = fmap tanh;
    asinh = fmap asinh; acosh = fmap acosh; atanh = fmap atanh;
  }

-- | Evaluate a 'TFRand' function using a Twofish pseudo-random seed composed of any four 64-bit
-- unsigned integers. The pure random result is returned.
seedEvalTFRand :: TFRandSeed -> TFRand a -> a
seedEvalTFRand seed f = evalTFRand f (tfGen seed)

-- | Evaluate the 'runTFRandSeed' function using entropy pulled from the operating system as a seed
-- value. This will produce a different random result every time it is run.
seedIOEvalTFRand :: TFRand a -> IO a
seedIOEvalTFRand f = evalTFRand f <$> initTFGen

seedIOEvalTFRandT :: (Monad m, MonadIO m) => TFRandT m a -> m a
seedIOEvalTFRandT f = liftIO initTFGen >>= evalTFRandT f

-- | Similar to 'evalTFRand', except instead of supplying just any 'TFRand' function for
-- evaluation, use the 'arbitrary' function intance that has been defined for the the data type @a@
-- to produce a result @a@.
arbTFRand :: Arbitrary a => TFRandSeed -> a
arbTFRand seed = evalTFRand arbitrary (tfGen seed)

-- | Similar to 'evalTFRand', except instead of supplying just any 'TFRand' function for
-- evaluation, use the 'arbitrary' function intance that has been defined for the the data type @a@
-- to produce a result @a@.
seedIOArbTFRand :: Arbitrary a => IO a
seedIOArbTFRand = seedIOEvalTFRand arbitrary

-- | Run a 'TFRand' function with an already-existing Twofish generator. This function is not very
-- useful unless you choose to use the 'System.Random.split' function to evaluate a nested 'TFRand'
-- function within another 'TFRand' function. If you simply want to generate a random value, it is
-- better to use 'runTFRandSeed' or 'runTFRandIO'.
evalTFRand :: TFRand a -> TFGen -> a
evalTFRand (TFRandT f) = evalRand f

-- | Like 'evalTFRand' but does not disgard the Twofish random generator, allowing you to re-use it
-- elsewhere.
runTFRand :: TFRand a -> TFGen -> (a, TFGen)
runTFRand (TFRandT f) = runRand f

-- | Evaluate a 'TFRandT' and discard the generator that initialized it.
evalTFRandT :: Monad m => TFRandT m a -> TFGen -> m a
evalTFRandT (TFRandT f) = evalRandT f

-- | Evaluate a 'TFRandT'.
runTFRandT :: Monad m => TFRandT m a -> TFGen -> m (a, TFGen)
runTFRandT (TFRandT f) = runRandT f

-- | Produce a 'TFGen' in the @IO@ monad and then use it to evaluate the 'TFRandT' function.
seedEvalTFRandT :: Monad m => TFRandSeed -> TFRandT m a -> m a
seedEvalTFRandT seed = liftM fst . flip runTFRandT (tfGen seed)

-- | Produce a 'TFGen' in the @IO@ monad and then use it to evaluate the 'TFRandT' function,
-- returning the generator for later use.
seedIORunTFRandT :: (Monad m, MonadIO m) => TFRandT m a -> m (a, TFGen)
seedIORunTFRandT f = liftIO initTFGen >>= runTFRandT f

seedIORunTFRand :: TFRand a -> IO (a, TFGen)
seedIORunTFRand f = runTFRand f <$> initTFGen

seedRunTFRand :: TFRandSeed -> TFRand a -> (a, TFGen)
seedRunTFRand = flip runTFRand . tfGen

seedRunTFRandT :: Monad m => TFRandSeed -> TFRandT m a -> m (a, TFGen)
seedRunTFRandT = flip runTFRandT . tfGen

----------------------------------------------------------------------------------------------------

-- | A function for testing a 'CPDFunction' using a 'CumulativeDistributionTable'. This function
-- creates an immutable 'Unboxed.Vector' with a number of "buckets" evenly-spaced along the
-- 'Unboxed.Vector' indicies, then uses a fair random number generator to produce random values and
-- applying each to the 'inverseTransformSample' function with the 'CumulativeDistributionTable' to
-- generate random index values distributed according to the bias in the
-- 'CumulativeDistributionTable'. Each biased random index is then treated as a counter token used
-- to increment the index of the associated "bucket" (as if the token is placed in the bucket) in
-- the 'Unboxed.Vector'. The result is a normalized 'Unboxed.Vector', where the maximum index in the
-- vector is @1.0@ and all elements in the array have been divided by the number of token counters
-- in the bucket with the most token counters.
testDistributionFunction
  :: Int -- ^ The size of the 'Unboxed.Vector' to create.
  -> Int -- ^ The number of random indicies to generate
  -> CumulativeDistributionTable
  -> IO (Unboxed.Vector ProcGenFloat)
testDistributionFunction size count table = do
  mvec <- Mutable.new size :: IO (Mutable.IOVector Int)
  let loop maxval count = if count <= 0 then return maxval else do
        i <- onRandFloat $ round . ((realToFrac (size - 1)) *) . inverseTransformSample table
        e <- (1 +) <$> liftIO (Mutable.read mvec i)
        liftIO $ Mutable.write mvec i (e + 1)
        (loop $! max e maxval) $! count - 1
  (maxval, _) <- initTFGen >>= runTFRandT (loop 0 count)
  normIOtoST mvec (realToFrac maxval) 0 (Mutable.new (Mutable.length mvec))

-- not for export
normIOtoST
  :: Mutable.IOVector Int
  -> ProcGenFloat -- maxval
  -> Int -- index
  -> (forall s . ST s (Mutable.STVector s ProcGenFloat))
  -> IO (Unboxed.Vector ProcGenFloat)
normIOtoST mvec maxval i nvecst = seq nvecst $!
  if i == Mutable.length mvec then evaluate (Unboxed.create nvecst) else do
    e <- Mutable.read mvec i
    normIOtoST mvec maxval (i + 1) $ do
      nvec <- nvecst
      Mutable.write nvec i $! realToFrac e / maxval
      return nvec
