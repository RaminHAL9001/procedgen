module ProcGen.Music.KeyFreq88
  (-- * Tonics
    ToneIndex(..), ToneShift(..), UniqueToneIndex(..), OctaveIntervalIndex,
    packW8, unpackW8, toneToUnique, uniqueToTone, toneShift, toneName, uniqueToneIndex,
    isUniqueToneIndex,
    -- * The 88-Key Piano Keyboard
    KeyIndex, keyIndex, keyIndexToWord8,
    keyboard88, the88Keys, concertA, concertAPianoKey, keysPerOctave, keyFreq, keyFreq',
    -- * Chords
    KeySignature(..), keySignature, keySigFreqTable,
    NamedChord(..), major7ths, minor7ths, augmented7ths, diminished7ths, dominant7ths,
    Chord, triad, seventh, nameToChord, chordToKeys
  )
  where

import           ProcGen.Types

import           Control.Arrow
                
import           Data.Char
import           Data.List                      (nub, sort)
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

newtype KeyIndex = KeyIndex { keyIndexToWord8 :: Word8 }
  deriving (Eq, Ord, Show, Read)

-- | Construct a 'KeyIndex', evaluates to an error value if the given
-- number is less than zero or greater than 87.
keyIndex :: Int -> KeyIndex
keyIndex i = if 0 <= i && i < 88 then KeyIndex $ fromIntegral i else error $
  "Keyboard88.keyIndex "++show i++" out of bounds: value must be between 0 and 87"

-- | The table of key frequencies in an 88-key keyboard. The lowest
-- value, A0 (27.5 Hz) is at @keyIndex 0@.
the88Keys :: Unboxed.Vector Frequency
the88Keys = Unboxed.fromList $ keyFreq <$> [0..87]

-- | Lookup a 'ProcGen.Types.Frequency' for the given 'KeyIndex'.
keyboard88 :: KeyIndex -> Frequency
keyboard88 (KeyIndex i) = the88Keys Unboxed.! fromIntegral i

concertA :: Frequency
concertA = 440.0

-- | In a 0-indexed array, concert A is the 48th key on the keyboard, obviously because the lowest
-- key is A which is index 0, each octave is 12 keys, and concert A is 4 octaves from the lowest key
-- on the keyboard.
concertAPianoKey :: KeyIndex
concertAPianoKey = KeyIndex 48

keysPerOctave :: Int
keysPerOctave = 12

keyFreq :: Frequency -> Frequency
keyFreq i = concertA * 2.0**((i - k) / d) where
  k = fromIntegral $ keyIndexToWord8 concertAPianoKey
  d = fromIntegral keysPerOctave

keyFreq' :: Frequency -> Frequency
keyFreq' f = k + d * log(f / concertA) / log 2 where
  k = fromIntegral $ keyIndexToWord8 concertAPianoKey
  d = fromIntegral keysPerOctave

----------------------------------------------------------------------------------------------------

data KeySignature = KeySignature !ToneIndex !Chord
  deriving (Eq, Ord, Show, Read)

-- | A constructor for a 'KeySignature', but unlike the capitalized data constructor, this function
-- takes a 'NamedChord' rather than a 'Chord'.
keySignature :: ToneIndex -> NamedChord -> KeySignature
keySignature i = KeySignature i . nameToChord

-- | Compute a 'KeySignature' frequency table, that is a table of all frequencies that are members
-- of this 'KeySignature'. A procedurally generated song will randomly select frequencies from a
-- table for the key signature. It is best to memoize these tables whenever possible.
keySigFreqTable :: KeySignature -> Unboxed.Vector Frequency
keySigFreqTable (KeySignature tonic chord) =
  let offset = fromIntegral $ fromEnum $ toneToUnique tonic in Unboxed.fromList
  [keyboard88 $ KeyIndex $ i + offset | (KeyIndex i) <- chordToKeys chord, i + offset < 88]

----------------------------------------------------------------------------------------------------

-- | Common names for chords. These are atomic symbols which instantiate the 'Prelude.Enum' type
-- class, but you can convert these to 'Chord' values using the 'nameToChord' function.
data NamedChord
  = Maj3 | Min3 | Dim3 | Sus3
  | Maj7 | Min7 | Dim7
  | HalfDim7 | DimMaj7 | MinMaj7 | AugMaj7 | AugMin7
  | Alt7 | Dom7 | DomFlat7
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

major7ths :: [NamedChord]
major7ths = [Maj7, DimMaj7, MinMaj7, AugMaj7]

minor7ths :: [NamedChord]
minor7ths = [Min7, MinMaj7, AugMin7]

augmented7ths :: [NamedChord]
augmented7ths = [AugMaj7, AugMin7]

diminished7ths :: [NamedChord]
diminished7ths = [Dim7, DimMaj7, HalfDim7]

dominant7ths :: [NamedChord]
dominant7ths = [Dom7, DomFlat7]

----------------------------------------------------------------------------------------------------

-- | These are chords which determine the key of a chord progression. Whether a chord is 'major' or
-- 'minor' depends on the integer values set in these constructors. The 'KeySignature' of the chord
-- independent of this chord value.
data Chord
  = Triad   !Word8 !Word8
  | Seventh !Word8 !Word8 !Word8
  deriving (Eq, Ord, Read, Show)

-- | A 'Triad' has 3 notes, the first note (tonic) is always 0, the next two are the number of
-- steps on a piano keyboard (counting both black and white keys) away from the tonic are the
-- next two notes.
triad :: Word8 -> Word8 -> Chord
triad a b = Triad (a `mod` 12) (b `mod` 12)

-- | Like 'triad' but takes an additional note.
seventh :: Word8 -> Word8 -> Word8 -> Chord
seventh a b c = Seventh (a `mod` 12) (b `mod` 12) (c `mod` 12)

-- | Produce all key indicies for a 'Chord', which you will usually produce from a 'nameToChord',
-- although you can invent your own 'Chord's which don't have an associated 'NamedChord'.
chordToKeys :: Chord -> [KeyIndex]
chordToKeys = (\ case { Triad a b -> [0, a, b]; Seventh a b c -> [0, a, b, c]; }) >>> \ keys ->
  fmap KeyIndex $ sort $ nub $ takeWhile (< 88) [m*12 + k | m <- [0 .. 7], k <- keys]

-- | Produce a 'Chord' from a 'NamedChord'. This function makes computing frequencies for
-- 'KeySignatures' much easier.
nameToChord :: NamedChord -> Chord
nameToChord = \ case
   Maj3     -> Triad 4 7 -- C  E  G
   Min3     -> Triad 3 7 -- C  Eb G
   Dim3     -> Triad 3 6 -- C  Eb Gb
   Sus3     -> Triad 5 7 -- C  F  G
   Maj7     -> Seventh 4 7 11 -- C  E  G  B
   Min7     -> Seventh 3 7 10 -- C  Eb G  Bb
   Dim7     -> Seventh 3 6  9 -- C  Eb Gb Bbb
   HalfDim7 -> Seventh 3 6 10 -- C  Eb Gb Bb
   DimMaj7  -> Seventh 3 6 11 -- C  Eb Gb B
   MinMaj7  -> Seventh 3 7 11 -- C  Eb G  B
   AugMaj7  -> Seventh 4 8 11 -- C  E  G# B
   AugMin7  -> Seventh 4 8 10 -- C  E  G# Bb
   Alt7     -> Seventh 4 8 10 -- C  E  G# Bb
   Dom7     -> Seventh 4 7 10 -- C  E  G  Bb
   DomFlat7 -> Seventh 5 7 11 -- C  E# G  B
