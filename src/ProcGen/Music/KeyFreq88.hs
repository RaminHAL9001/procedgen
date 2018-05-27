module ProcGen.Music.KeyFreq88
  ( OctaveIntervalIndex, ToneShift(..), ToneIndex(..), UniqueToneIndex(..),
    packW8, unpackW8, toneToUnique, uniqueToTone, toneShift, toneName, uniqueToneIndex,
    isUniqueToneIndex,
    KeyIndex, keyIndexToInt,
    keyboard88, the88Keys, concertA, concertAPianoKey, keysPerOctave, keyFreq, keyFreq',
  )
  where

import           ProcGen.Types

import           Control.Arrow
                
import           Data.Char
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable
                
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | A standard 88-key piano keyboard is a single-rank keyboard with 7 and 1/3 twelve-note octave
-- intervals. The 'OctaveIntervalIndex' index selects one of the 7 and 1/3 (rounded up to 8) octave
-- intervals.
type OctaveIntervalIndex = Int

packW8 :: (Show e, Enum e) => e -> Word8
packW8 e = let i = fromEnum e in
  if i <= fromIntegral (maxBound::Word8) then fromIntegral i else error $
    "the value " ++ show e ++
    " is enumerated from " ++ show i ++
    " which cannot be converted to a Word8 limited to the range [" ++ show (minBound :: Word8) ++
    ".." ++ show (maxBound::Word8) ++ "]"

unpackW8 :: Enum e => Word8 -> e
unpackW8 = toEnum . fromIntegral

----------------------------------------------------------------------------------------------------

data ToneShift = Natural | Sharp | Flat deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | This is a logical tone index. This is different from a 'UniqueToneIndex' where each symbol maps
-- to a unique frequency. A 'ToneIndex' is a logical symbol, so 'C'' maps to the same tonal
-- frequency as 'D_'. Therefore this symbol should not be used for computing chords. Use
-- 'UniqueToneIndex' to compute chords.
data ToneIndex
  = A_ | A | A'
  | B_ | B | B'
  | C_ | C | C'
  | D_ | D | D'
  | E_ | E | E'
  | F_ | F | F'
  | G_ | G | G'
  deriving (Eq, Ord, Enum, Bounded)

-- | An 'UniqueToneIndex' has one symbol for each of the 11 units in a piano scale, which is unlike
-- the 'ToneIndex' where (for example) C-sharp maps to the same frequency as a D-flat. For a
-- 'UniqueToneIndex' the mapping from symbol to frequency is 1-to-1. The naming convention is
-- standard A-G, with the prime character @(')@ indicating a "sharp" note. So @A@ indicates the A
-- note, @A'@ indicates A sharp.
data UniqueToneIndex
  = Au | Au' | Bu | Cu | Cu' | Du | Du' | Eu | Fu | Fu' | Gu | Gu'
  deriving (Eq, Ord, Enum, Bounded)

instance Show UniqueToneIndex where
  show = show . uniqueToTone

instance Read ToneIndex where
  readsPrec _p str = do
    let vec = toneIndexTable
    (note, str) <- case str of
      c:str | 'A'<=c && c<='G' -> [(vec Unboxed.! (ord c - ord 'A'), str)]
      c:str | 'a'<=c && c<='g' -> [(vec Unboxed.! (ord c - ord 'a'), str)]
      c:_                      -> error $ show c ++ " is not a valid note"
      ""                       -> []
    (accent, str) <- case str of
      c:str | ord c==9837 || c == '_' -> [(negate 1, str)]
      c:str | ord c==9839 || c == '#' -> [(1, str)]
      c:str | ord c==9838             -> [(0, str)]
      str                             -> [(0, str)]
    let i = fromIntegral (note+accent)
    if isUniqueToneIndex i then [(toEnum i, dropWhile isSpace str)] else
      error $ "value out of bounds, octaves contain only 12 indicies"

instance Show ToneIndex where
  show i = toneName i : case toneShift i of
    Natural -> ""
    Flat    -> "_"
    Sharp   -> "#"

isUniqueToneIndex :: Int -> Bool
isUniqueToneIndex i =
  fromEnum (minBound :: UniqueToneIndex) <= i &&
  i <= fromEnum (maxBound :: UniqueToneIndex)

toneIndexTable :: Unboxed.Vector Word8
toneIndexTable = Unboxed.fromList $ packW8 <$> [A, B, C, D, E, F, G]

makeToneIndexTable
  :: forall a e i . (Bounded i, Enum i, Mutable.Unbox e)
  => (a -> e) -> [(i, a)] -> Unboxed.Vector e
makeToneIndexTable pack elems = Unboxed.create $ do
  vec <- Mutable.new $ fromEnum (maxBound :: i) - fromEnum (minBound :: i)
  mapM_ (uncurry $ Mutable.write vec) $ (fromEnum *** pack) <$> elems
  return vec

toneLogicalMap :: [(ToneIndex, UniqueToneIndex)]
toneLogicalMap =
  [(A_,Gu'),(A,Au),(A',Au')
  ,(B_,Au'),(B,Bu),(B',Cu )
  ,(C_,Bu ),(C,Cu),(C',Cu')
  ,(D_,Cu'),(D,Du),(D',Du')
  ,(E_,Du'),(E,Eu),(E',Fu )
  ,(F_,Eu ),(F,Fu),(F',Fu')
  ,(G_,Fu'),(G,Gu),(G',Gu')
  ]

toneUniqueTable :: Unboxed.Vector Word8
toneUniqueTable = makeToneIndexTable packW8 toneLogicalMap

uniqueToneTable :: Unboxed.Vector Word8
uniqueToneTable = makeToneIndexTable packW8
  [(Au,A),(Au',A')
  ,(Bu,B)
  ,(Cu,C),(Cu',C')
  ,(Du,D),(Du',D')
  ,(Eu,E)
  ,(Fu,F),(Fu',F')
  ,(Gu,G),(Gu',G')
  ]

toneShiftTable :: Unboxed.Vector Word8
toneShiftTable = makeToneIndexTable packW8
  [(A_,Flat),(A,Natural),(A',Sharp)
  ,(B_,Flat),(B,Natural),(B',Sharp)
  ,(C_,Flat),(C,Natural),(C',Sharp)
  ,(D_,Flat),(D,Natural),(D',Sharp)
  ,(E_,Flat),(E,Natural),(E',Sharp)
  ,(F_,Flat),(F,Natural),(F',Sharp)
  ,(G_,Flat),(G,Natural),(G',Sharp)
  ]

toneNameTable :: Unboxed.Vector Char
toneNameTable = makeToneIndexTable id
  [(A_,'A'),(A,'A'),(A','A')
  ,(B_,'B'),(B,'B'),(B','B')
  ,(C_,'C'),(C,'C'),(C','C')
  ,(D_,'D'),(D,'D'),(D','D')
  ,(E_,'E'),(E,'E'),(E','E')
  ,(F_,'F'),(F,'F'),(F','F')
  ,(G_,'G'),(G,'G'),(G','G')
  ]

-- | Convert a 'ToneIndex' to a 'UniqueToneIndex'.
toneToUnique :: ToneIndex -> UniqueToneIndex
toneToUnique = toEnum . fromIntegral . (toneUniqueTable Unboxed.!) . fromEnum

uniqueToTone :: UniqueToneIndex -> ToneIndex
uniqueToTone = toEnum . fromIntegral . (uniqueToneTable Unboxed.!) . fromEnum

-- | Construct an 'UniqueToneIndex' value from an 'Prelude.Integral' value modulo the and
-- value @('Prelude.fromEnum' 'Prelude.maxBound')@, so the constructor is guaranteed to create a
-- valid 'UniqueToneIndex' regardless of the value given.
uniqueToneIndex :: (Num i, Integral i) => i -> UniqueToneIndex
uniqueToneIndex = toEnum . fromIntegral . flip mod 12

-- | Return the 'ToneShift' for the given 'ToneIndex'.
toneShift :: ToneIndex -> ToneShift
toneShift = unpackW8 . (toneShiftTable Unboxed.!) . fromEnum

-- | Return the western character symbol for the given 'ToneIndex'.
toneName :: ToneIndex -> Char
toneName = (toneNameTable Unboxed.!) . fromEnum

----------------------------------------------------------------------------------------------------

newtype KeyIndex = KeyIndex { keyIndexToInt :: Int }
  deriving (Eq, Ord, Show, Read)

the88Keys :: Unboxed.Vector Frequency
the88Keys = Unboxed.fromList $ keyFreq <$> [0..87]

-- | Lookup a 'ProcGen.Types.Frequency' for the given 'KeyIndex'.
keyboard88 :: Int -> Frequency
keyboard88 i = the88Keys Unboxed.! fromIntegral i

concertA :: Frequency
concertA = 440.0

-- | In a 0-indexed array, concert A is the 48th key on the keyboard, obviously because the lowest
-- key is A which is index 0, each octave is 12 keys, and concert A is 4 octaves from the lowest key
-- on the keyboard.
concertAPianoKey :: Int
concertAPianoKey = 48

keysPerOctave :: Int
keysPerOctave = 12

keyFreq :: Frequency -> Frequency
keyFreq i = concertA * 2.0**((i - k) / d) where
  k = fromIntegral concertAPianoKey
  d = fromIntegral keysPerOctave

keyFreq' :: Frequency -> Frequency
keyFreq' f = k + d * log(f / concertA) / log 2 where
  k = fromIntegral concertAPianoKey
  d = fromIntegral keysPerOctave
