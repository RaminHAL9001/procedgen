-- | A scripting language used within the ProcGen application. In the same way you can implement a
-- Lisp interpreter with a few lines of Lisp, you can implement a Haskell interpreter with a few
-- (hundred) lines of Haskell.
--
-- The scripting language defined in this module is essentially for declaring mathematical equations
-- that can be easily serialized, optimized, and interpreted. The scripting language can also be
-- procedurally generated.
module ProcGen.Script where

import           ProcGen.Arbitrary
import           ProcGen.Types

import           Control.Applicative

import           Data.Ratio

----------------------------------------------------------------------------------------------------

type FloatFunction = RealValueFunction ProcGenFloat

-- | A small DSL (or perhaps a "meta function") for functions over 'Float' values. Use this data
-- type when you have a mathematical equation that you want to serialze or store to a file.
--
-- The disadvantage to using this data type is that you lose the ability to optimize computations.
-- It is usually a good idea extend this data type with your own functions. For example, suppose you
-- want to construct a 'Polynomial' data type, you can create your own data type which efficiently
-- computes polynomials, but uses this 'RealValueFunction' type as the polynomial coefficients. This
-- would result in be a much, much more efficient computations than defining polynomials in terms of
-- 'RealValueFunction's using the 'FFPow', 'FFMul', 'FFAdd', 'FFNegate' functions.
data RealValueFunction num
  = FFConst    !num -- ^ a constant value
  | FFFloat    !Float
  | FFDouble   !Double
  | FFInteger  !Integer
  | FFRatio    !Integer !Integer
  | FFUniformRand -- ^ A unformly distributed random variable between 0 and 1
  | FFNormalRand
     -- ^ A normally distributed random variable between 0 and 1, with 0.5 being the most common
     -- value.
  | FFBeta3Rand
     -- ^ A beta-distributed random variable with a beta function of polynomial degree 3, generating
     -- a value between 0 and 1 with @(1/3)@ being the value most common generated.
  | FFBeta5Rand
     -- ^ A beta-distributed random variable with a beta function of polynomial degree 5, generating
     -- a value between 0 and 1 with @(1/5)@ being the value most commonly generated.
  | FFAdd        (RealValueFunction num)  (RealValueFunction num)
  | FFMul        (RealValueFunction num)  (RealValueFunction num)
  | FFMin        (RealValueFunction num)  (RealValueFunction num)
  | FFMod        (RealValueFunction num)  (RealValueFunction num)
  | FFMax        (RealValueFunction num)  (RealValueFunction num)
  | FFToFloat    (RealValueFunction num)
    -- ^ evaluates a function to a 'Float' constant
  | FFToDouble   (RealValueFunction num)
    -- ^ evaluates a function to a 'Double' constant
  | FFRound      (RealValueFunction num)
    -- ^ evaluates a function to an 'Integer' constant by rounding
  | FFFloor      (RealValueFunction num)
    -- ^ evaluates a function to an 'Integer' constant by taking the 'floor'
  | FFCeiling    (RealValueFunction num)
    -- ^ evaluates a function to an 'Integer' constant by taking the 'ceiling'
  | FFNegate     (RealValueFunction num)
  | FFAbs        (RealValueFunction num)
  | FFSignNum    (RealValueFunction num)
  | FFRecip      (RealValueFunction num)
  | FFExp        (RealValueFunction num)
    -- ^ Take the constant @e@ to the power of a value
  | FFSqrt       (RealValueFunction num)
  | FFLog        (RealValueFunction num)
    -- ^ Take the natural logarithm of a value
  | FFPow        (RealValueFunction num)  (RealValueFunction num)
  | FFLogBase    (RealValueFunction num)  (RealValueFunction num)
  | FFSin        (RealValueFunction num)
  | FFCos        (RealValueFunction num)
  | FFTan        (RealValueFunction num)
  | FFASin       (RealValueFunction num)
  | FFACos       (RealValueFunction num)
  | FFATan       (RealValueFunction num)
  | FFSinH       (RealValueFunction num)
  | FFCosH       (RealValueFunction num)
  | FFTanH       (RealValueFunction num)
  | FFASinH      (RealValueFunction num)
  | FFACosH      (RealValueFunction num)
  | FFATanH      (RealValueFunction num)
  | FFReLU       (RealValueFunction num)
  | FFLeakyReLU  (RealValueFunction num) (RealValueFunction num)
  | FFSawtooth   (RealValueFunction num)
  | FFTriangle   (RealValueFunction num)
  | FFSquare     (RealValueFunction num)
  | FFUnitSine   (RealValueFunction num)
  | FFClamp0_1   (RealValueFunction num)
  | FFClamp1_1   (RealValueFunction num)
  | FFBezier3    (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
  | FFSlope      (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
  | FFSigmoid    (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
  | FFSine2      (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
  | FFFadeInOut  (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
  | FFSinePulse  (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
  | FFSinePulse3 (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
                 (RealValueFunction num)
  | FFNormal     (RealValueFunction num) (RealValueFunction num)
  | FFBeta       (RealValueFunction num) (RealValueFunction num)
  deriving (Eq, Ord, Functor)

instance Ord num => MinMaxDomain (RealValueFunction num) where
  minOf = FFMin
  maxOf = FFMax

instance Num (RealValueFunction num) where
  (+) a b     = FFAdd a b
  (-) a b     = FFAdd a (FFNegate b)
  (*) a b     = FFMul a b
  negate      = FFNegate
  abs         = FFAbs
  signum      = FFSignNum
  fromInteger = FFInteger

instance Fractional (RealValueFunction num) where
  (/) a b        = FFMul a $ FFRecip b
  recip          = FFRecip
  fromRational a = FFRatio (numerator a) (denominator a)

instance Floating num => Floating (RealValueFunction num) where
  pi      = FFConst pi
  exp     = FFExp
  log     = FFLog
  sqrt    = FFSqrt
  (**)    = FFPow
  logBase = FFLogBase
  sin     = FFSin
  cos     = FFCos
  tan     = FFTan
  asin    = FFASin
  acos    = FFACos
  atan    = FFATan
  sinh    = FFSinH
  cosh    = FFCosH
  tanh    = FFTanH
  asinh   = FFASinH
  acosh   = FFACosH
  atanh   = FFATanH

instance Ord num => ClampedDomain (RealValueFunction num) where
  clamp0_1 = FFClamp0_1
  clamp1_1 = FFClamp1_1

instance (Ord num, RealFrac num) => ModulousDomain (RealValueFunction num) where
  contMod = FFMod

instance RoundingDomain (RealValueFunction num) where
  roundDown = FFFloor
  roundUp   = FFCeiling
  roundMid  = FFRound

instance PeriodicDomain num => PeriodicDomain (RealValueFunction num) where
  unitSine = FFUnitSine
  sawtooth = FFSawtooth
  triangle = FFTriangle
  square   = FFSquare

instance EnvelopeDomain num => EnvelopeDomain (RealValueFunction num) where
  slope       (TimeWindow a b) = FFSlope     a b
  sigmoid     (TimeWindow a b) = FFSigmoid   a b
  sineSquared (TimeWindow a b) = FFSine2     a b
  fadeInOut                    = FFFadeInOut

instance ProbabilityDomain num => ProbabilityDomain (RealValueFunction num) where
  normal = FFNormal
  beta   = FFBeta

instance PulsedSinusoidalDomain num => PulsedSinusoidalDomain (RealValueFunction num) where
  sinePulse  = FFSinePulse
  sinePulse3 = FFSinePulse3

-- | Force computation of equation as much as possible without forcing evaluation of any of the
-- random variables.
reduceRealValue
  :: forall num
     . (RealFrac num, Floating num,
        EnvelopeDomain num, ClampedDomain num, ModulousDomain num,
        PeriodicDomain num, ActivationDomain num, PulsedSinusoidalDomain num)
  => RealValueFunction num -> RealValueFunction num
reduceRealValue a = maybe a id $ reduce a where
  loop = reduceRealValue
  reduce = \ case
    a@FFConst{}       -> pure a
    a@FFInteger{}     -> pure a
    a@FFFloat{}       -> pure a
    a@FFDouble{}      -> pure a
    a@FFRatio{}       -> pure a
    a@FFUniformRand{} -> pure a
    a@FFNormalRand{}  -> pure a
    a@FFBeta3Rand{}   -> pure a
    a@FFBeta5Rand{}   -> pure a
    FFToFloat  a      -> eval1 FFToFloat  (toFloat FFFloat)  a
    FFToDouble a      -> eval1 FFToDouble (toFloat FFDouble) a
    FFRound    a      -> eval1 FFRound (toInt round) a
    FFFloor    a      -> eval1 FFFloor (toInt floor) a
    FFCeiling  a      -> eval1 FFCeiling (toInt ceiling) a
    FFAdd     a b     -> eval2 FFAdd (real2 (+)) a b
    FFMul     a b     -> eval2 FFMul (real2 (*)) a b
    FFMod     a b     -> eval2 FFMod (float2 contMod) a b
    FFMax     a b     -> eval2 FFMax (real2 maxOf) a b
    FFMin     a b     -> eval2 FFMin (real2 minOf) a b
    FFNegate  a       -> eval1 FFNegate (real1 negate) a
    FFAbs     a       -> eval1 FFAbs (real1 abs) a
    FFSignNum a       -> eval1 FFSignNum (real1 signum) a
    FFRecip   a       -> eval1 FFRecip (float1 recip) a
    FFExp     a       -> eval1 FFExp (float1 exp) a
    FFLog     a       -> eval1 FFLog (float1 log) a
    FFSqrt    a       -> eval1 FFSqrt (float1 sqrt) a
    FFPow     a b     -> eval2 FFPow (float2 (**)) a b
    FFLogBase a b     -> eval2 FFLogBase (float2 logBase) a b
    FFSin     a       -> eval1 FFSin (float1 sin) a
    FFCos     a       -> eval1 FFCos (float1 cos) a
    FFTan     a       -> eval1 FFTan (float1 tan) a
    FFASin    a       -> eval1 FFASin (float1 asin) a
    FFACos    a       -> eval1 FFACos (float1 acos) a
    FFATan    a       -> eval1 FFATan (float1 atan) a
    FFSinH    a       -> eval1 FFSinH (float1 sinh) a
    FFCosH    a       -> eval1 FFCosH (float1 cosh) a
    FFTanH    a       -> eval1 FFTanH (float1 tanh) a
    FFASinH   a       -> eval1 FFASinH (float1 asinh) a
    FFACosH   a       -> eval1 FFACosH (float1 acosh) a
    FFATanH   a       -> eval1 FFATanH (float1 atanh) a
    FFReLU    a       -> eval1 FFReLU  (float1 relu) a
    FFLeakyReLU a b   -> eval2 FFLeakyReLU (float2 leakyReLU) a b
    FFSawtooth  a     -> eval1 FFSawtooth (float1 sawtooth) a
    FFTriangle  a     -> eval1 FFTriangle (float1 triangle) a
    FFSquare    a     -> eval1 FFSquare   (float1 square)   a
    FFUnitSine  a     -> eval1 FFUnitSine (float1 unitSine) a
    FFClamp0_1  a     -> eval1 FFClamp0_1 (float1 clamp0_1) a
    FFClamp1_1  a     -> eval1 FFClamp1_1 (float1 clamp1_1) a
    FFNormal    a b   -> eval2 FFNormal   (float2 normal)  a b
    FFBeta      a b   -> eval2 FFBeta     (float2 beta)    a b
    FFSlope     a b c -> eval3 FFSlope    (envel slope) a b c
    FFSigmoid   a b c -> eval3 FFSigmoid  (envel sigmoid) a b c
    FFSine2     a b c -> eval3 FFSine2    (envel sineSquared) a b c
    FFBezier3   a b c d t -> eval5 bezier3 FFBezier3 a b c d t
    FFFadeInOut a b c d t -> eval5 fadeInOut FFFadeInOut a b c d t
    FFSinePulse a' b'  t' -> let { a = loop a'; b = loop b'; t = loop t'; } in
      FFConst  <$> (sinePulse <$> getConst a <*> getConst b <*> getConst t) <|>
      FFFloat  <$> (sinePulse <$> getFloat a <*> getFloat b <*> getFloat t) <|>
      FFDouble <$> (sinePulse <$> getDouble a <*> getDouble b <*> getDouble t) <|>
      FFDouble <$> (sinePulse <$> getAnyDouble a <*> getAnyDouble b <*> getAnyDouble t) <|>
      pure (FFSinePulse a b t)
    FFSinePulse3 a' b' c' t' -> let { a = loop a'; b = loop b'; c = loop c'; t = loop t'; } in
      FFConst  <$> (sinePulse3 <$> getConst a <*> getConst b <*> getConst c <*> getConst t) <|>
      FFFloat  <$> (sinePulse3 <$> getFloat a <*> getFloat b <*> getFloat c <*> getFloat t) <|>
      FFDouble <$> (sinePulse3 <$> getDouble a <*> getDouble b <*> getDouble c <*> getDouble t) <|>
      FFDouble <$> (sinePulse3 <$> getAnyDouble a <*> getAnyDouble b <*> getAnyDouble c <*> getAnyDouble t) <|>
      pure (FFSinePulse3 a b c t)
  ffratio r    = FFRatio (numerator r) (denominator r)
  getConst     = \ case { FFConst   a -> pure a; _ -> empty; }
  getInteger   = \ case { FFInteger a -> pure a; _ -> empty; }
  getFloat     = \ case { FFFloat   a -> pure a; _ -> empty; }
  getDouble    = \ case { FFDouble  a -> pure a; _ -> empty; }
  getRatio     = \ case { FFRatio a b -> pure (a % b); _ -> empty; }
  getAnyDouble = \ case
    FFDouble a -> pure a
    FFFloat  a -> pure $ realToFrac a
    _          -> empty
  getAnyRatio  = \ case
    FFRatio a b -> pure $ a % b
    FFInteger a -> pure $ a % 1
    FFDouble  a -> pure $ realToFrac a
    FFFloat   a -> pure $ realToFrac a
    _           -> empty
  toFloat constr = \ case
    FFConst   a -> pure $ constr $ realToFrac a
    FFFloat   a -> pure $ constr $ realToFrac a
    FFDouble  a -> pure $ constr $ realToFrac a
    FFInteger a -> pure $ constr $ realToFrac $ a % 1
    FFRatio a b -> pure $ constr $ realToFrac $ a % b
    _           -> empty
  toInt
    :: (forall n . RealFrac n => n -> Integer)
    -> RealValueFunction num -> Maybe (RealValueFunction num)
  toInt round = \ case
    FFConst   a -> pure $ FFInteger $ round a
    FFFloat   a -> pure $ FFInteger $ round a
    FFDouble  a -> pure $ FFInteger $ round a
    FFInteger a -> pure $ FFInteger a
    FFRatio a b -> pure $ FFInteger $ round $ a % b
    _           -> empty
  --------------------------------------------------------------------------------
  get1Value
    :: (v -> v)
    -> (RealValueFunction num -> Maybe v)
    -> (v -> RealValueFunction num)
    -> RealValueFunction num -> Maybe (RealValueFunction num)
  get1Value f get constr a = constr . f <$> get a
  float1
    :: (forall n .
          (RealFrac n, Floating n,
           ActivationDomain n, PeriodicDomain n, ClampedDomain n)
          => n -> n
       )
    -> RealValueFunction num -> Maybe (RealValueFunction num)
  float1 f a =
    get1Value f getConst     FFConst   a <|>
    get1Value f getFloat     FFFloat   a <|>
    get1Value f getDouble    FFDouble  a <|>
    get1Value f getAnyDouble FFDouble  a
  ratio1 f a = 
    get1Value f getRatio     ffratio   a <|>
    get1Value f getAnyRatio  ffratio   a
  toFloat
    :: Fractional f
    => (f -> RealValueFunction num)
    -> RealValueFunction num -> Maybe (RealValueFunction num)
  real1
    :: (forall n . Real n => n -> n)
    -> RealValueFunction num -> Maybe (RealValueFunction num)
  real1 f a = get1Value f getInteger FFInteger a <|> float1 f a <|> ratio1 f a
  eval1
    :: (RealValueFunction num -> RealValueFunction num)
    -> (RealValueFunction num -> Maybe (RealValueFunction num))
    -> RealValueFunction num -> Maybe (RealValueFunction num)
  eval1 constr eval a' = let a = loop a' in eval a <|> pure (constr a)
  --------------------------------------------------------------------------------
  get2Values
    :: (v -> v -> v)
    -> (RealValueFunction num -> Maybe v)
    -> (v -> RealValueFunction num)
    -> RealValueFunction num -> RealValueFunction num -> Maybe (RealValueFunction num)
  get2Values   f get constr a b = constr <$> (f <$> get a <*> get b)
  float2
    :: (forall n .
          (RealFrac n, Floating n, ModulousDomain n, MinMaxDomain n,
           ActivationDomain n, ProbabilityDomain n)
          => n -> n -> n
       )
    -> RealValueFunction num -> RealValueFunction num -> Maybe (RealValueFunction num)
  float2 f a b =
    get2Values f getConst     FFConst   a b <|>
    get2Values f getFloat     FFFloat   a b <|>
    get2Values f getDouble    FFDouble  a b <|>
    get2Values f getAnyDouble FFDouble  a b
  ratio2 f a b =
    get2Values f getRatio     ffratio   a b <|>
    get2Values f getAnyRatio  ffratio   a b
  real2
    :: (forall n . (Real n, MinMaxDomain n) => n -> n -> n)
    -> RealValueFunction num -> RealValueFunction num -> Maybe (RealValueFunction num)
  real2 f a b = get2Values f getInteger FFInteger a b <|> float2 f a b <|> ratio2 f a b
  eval2
    :: (RealValueFunction num -> RealValueFunction num -> RealValueFunction num)
    -> (RealValueFunction num -> RealValueFunction num -> Maybe (RealValueFunction num))
    -> RealValueFunction num -> RealValueFunction num -> Maybe (RealValueFunction num)
  eval2 constr eval a' b' = do
    let a = loop a'
    let b = loop b'
    eval a b <|> pure (constr a b)
  --------------------------------------------------------------------------------
  envel
    :: (forall n . (RealFrac n, Floating n, EnvelopeDomain n) => TimeWindow n -> n -> n)
    -> RealValueFunction num -> RealValueFunction num -> RealValueFunction num
    -> Maybe (RealValueFunction num)
  envel f a b c =
    FFConst  <$> (f <$> (TimeWindow <$> getConst     a <*> getConst     b) <*> getConst     c) <|>
    FFFloat  <$> (f <$> (TimeWindow <$> getFloat     a <*> getFloat     b) <*> getFloat     c) <|>
    FFDouble <$> (f <$> (TimeWindow <$> getDouble    a <*> getDouble    b) <*> getDouble    c) <|>
    FFDouble <$> (f <$> (TimeWindow <$> getAnyDouble a <*> getAnyDouble b) <*> getAnyDouble c)
  eval3 constr eval a' b' c' = do
    let a = loop a'
    let b = loop b'
    let c = loop c'
    eval a b c <|> pure (constr a b c)
  --------------------------------------------------------------------------------
  eval5
    :: (forall n .
          (RealFrac n, Floating n, ActivationDomain n, EnvelopeDomain n)
           => n -> n -> n -> n -> n -> n
       )
    -> (   RealValueFunction num -> RealValueFunction num
        -> RealValueFunction num -> RealValueFunction num
        -> RealValueFunction num -> RealValueFunction num
       )
    -> RealValueFunction num -> RealValueFunction num
    -> RealValueFunction num -> RealValueFunction num
    -> RealValueFunction num -> Maybe (RealValueFunction num)
  eval5 f constr a' b' c' d' t' =
    let { a = loop a'; b = loop b'; c = loop c'; d = loop d'; t = loop t' } in
    FFConst  <$> (f <$> getConst a <*> getConst b <*> getConst c <*> getConst d <*> getConst t) <|>
    FFFloat  <$> (f <$> getFloat a <*> getFloat b <*> getFloat c <*> getFloat d <*> getFloat t) <|>
    FFDouble <$> (f <$> getDouble a <*> getDouble b <*> getDouble c <*> getDouble d <*> getDouble t) <|>
    FFDouble <$> (f <$> getAnyDouble a <*> getAnyDouble b <*> getAnyDouble c <*> getAnyDouble d <*> getAnyDouble t) <|>
    pure (constr a b c d t)
{-# SPECIALIZE reduceRealValue :: RealValueFunction ProcGenFloat -> RealValueFunction ProcGenFloat #-}

forceRealValue
  :: forall m num
     . (Monad m, MonadRandom m, RealFrac num, Floating num,
        EnvelopeDomain num, ClampedDomain num, ModulousDomain num,
        PeriodicDomain num, ActivationDomain num, PulsedSinusoidalDomain num)
  => RealValueFunction num -> m num
forceRealValue = let eval = forceRealValue in \ case
  FFConst        a         -> pure a
  FFInteger      a         -> pure $ realToFrac a
  FFFloat        a         -> pure $ realToFrac a
  FFDouble       a         -> pure $ realToFrac a
  FFRatio        a b       -> pure $ realToFrac $ a % b
  FFUniformRand            -> (realToFrac :: ProcGenFloat -> num) <$> getRandom
  FFNormalRand             -> realToFrac . inverseNormal <$> getRandom
  FFBeta3Rand              -> realToFrac . inverseBeta3  <$> getRandom
  FFBeta5Rand              -> realToFrac . inverseBeta5  <$> getRandom
  FFAdd          a b       -> (+)  <$> eval a <*> eval (realToFrac <$> b)
  FFMul          a b       -> (*)  <$> eval a <*> eval (realToFrac <$> b)
  FFMod          a b       -> contMod     <$> eval a <*> eval b
  FFMin          a b       -> minOf       <$> eval a <*> eval b
  FFMax          a b       -> maxOf       <$> eval a <*> eval b
  FFToFloat      a         -> realToFrac  <$> reduceFloat  (realToFrac <$> a)
  FFToDouble     a         -> realToFrac  <$> reduceDouble (realToFrac <$> a)
  FFRound        a         -> fromInteger . round   <$> reduceDouble (realToFrac <$> a)
  FFFloor        a         -> fromInteger . floor   <$> reduceDouble (realToFrac <$> a)
  FFCeiling      a         -> fromInteger . ceiling <$> reduceDouble (realToFrac <$> a)
  FFNegate       a         -> negate      <$> eval a
  FFAbs          a         -> abs         <$> eval a
  FFSignNum      a         -> signum      <$> eval a
  FFRecip        a         -> recip       <$> eval a
  FFExp          a         -> exp         <$> eval a
  FFLog          a         -> log         <$> eval a
  FFPow          a b       -> realToFrac  <$> ((**) <$> reduceDouble (realToFrac <$> a) <*> reduceDouble (realToFrac <$> b))
  FFLogBase      a b       -> realToFrac  <$> (logBase <$> reduceDouble (realToFrac <$> a) <*> reduceDouble (realToFrac <$> b))
  FFSin          a         -> sin         <$> eval a
  FFCos          a         -> cos         <$> eval a
  FFTan          a         -> tan         <$> eval a
  FFASin         a         -> asin        <$> eval a
  FFACos         a         -> acos        <$> eval a
  FFATan         a         -> atan        <$> eval a
  FFSinH         a         -> sinh        <$> eval a
  FFCosH         a         -> cosh        <$> eval a
  FFTanH         a         -> tanh        <$> eval a
  FFASinH        a         -> asinh       <$> eval a
  FFACosH        a         -> acosh       <$> eval a
  FFATanH        a         -> atanh       <$> eval a
  FFSqrt         a         -> sqrt        <$> eval a
  FFReLU         a         -> relu        <$> eval a
  FFLeakyReLU    a b       -> leakyReLU   <$> eval a <*> eval b
  FFSawtooth     a         -> sawtooth    <$> eval a
  FFTriangle     a         -> triangle    <$> eval a
  FFSquare       a         -> square      <$> eval a
  FFUnitSine     a         -> unitSine    <$> eval a
  FFSinePulse    a b     t -> sinePulse   <$> eval a <*> eval b <*> eval t
  FFSinePulse3   a b c   t -> sinePulse3  <$> eval a <*> eval b <*> eval c <*> eval t
  FFClamp0_1     a         -> clamp0_1    <$> eval a
  FFClamp1_1     a         -> clamp1_1    <$> eval a
  FFBezier3      a b c d t -> bezier3     <$> eval a <*> eval b <*> eval c <*> eval d <*> eval t
  FFSlope        a b     t -> slope       <$> (TimeWindow <$> eval a <*> eval b) <*> eval t
  FFSigmoid      a b     t -> sigmoid     <$> (TimeWindow <$> eval a <*> eval b) <*> eval t  
  FFSine2        a b     t -> sineSquared <$> (TimeWindow <$> eval a <*> eval b) <*> eval t
  FFFadeInOut    a b c d t -> fadeInOut   <$> eval a <*> eval b <*> eval c <*> eval d <*> eval t
  FFNormal       a       t -> normal      <$> eval a <*> eval t
  FFBeta         a       t -> beta        <$> eval a <*> eval t
{-# SPECIALIZE forceRealValue :: MonadRandom m => RealValueFunction ProcGenFloat -> m ProcGenFloat #-}

reduceDouble :: (Monad m, MonadRandom m) => RealValueFunction Double -> m Double
reduceDouble = forceRealValue

reduceFloat :: (Monad m, MonadRandom m) => RealValueFunction Float -> m Float
reduceFloat = forceRealValue
