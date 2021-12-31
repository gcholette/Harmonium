module Harmony where

infixr 5 :~:

type Semitones = Int

type Duration = Int

type NoteId = Int

type OctaveNumber = Int

data Scale
  = Major
  | Minor
  | HarmonicMinor
  | PentatonicMinor
  | MinorBlues
  | Ionian
  | Aeolian
  --  | Dorian
  --  | Phrygian
  --  | Lydian
  --  | Mixolodian
  --  | Locrian
  deriving (Show)

data NoteSymbol
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | As
  | Cs
  | Ds
  | Fs
  | Gs
  deriving (Show)

data Step = Semitone | Tone
  deriving (Show)

type PitchProps = (NoteSymbol, OctaveNumber)

newtype Pitch = Pitch PitchProps
  deriving (Show)

newtype Pitches = Pitches [PitchProps]
  deriving (Show)

type DegreeId = Int

data DegreeType = Reg | Sharp | Flat | DoubleSharp | DoubleFlat
  deriving (Show, Eq)

data Locality = Foreign | Local
  deriving (Show, Eq)

data Degree = Degree Locality DegreeType DegreeId
  deriving (Show, Eq)

type ScaleNote a = (a, Degree)

data ScaleBlueprint a = ScaleBlueprint a [Degree]
  deriving (Show)

data PitchedScale a b = PitchedScale NoteSymbol a [ScaleNote b]
  deriving (Show)

data Sequence a
  = Note Duration Pitch
  | Chord Duration Pitches
  | Rest Duration
  | Sequence a :~: Sequence a
  deriving (Show)

majorSteps :: [Step]
majorSteps = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

zipSteps :: [b] -> [(Step, b)]
zipSteps = zip majorSteps

moveNoteWithDegreeType :: NoteId -> DegreeType -> NoteId
moveNoteWithDegreeType n Reg = n
moveNoteWithDegreeType n Flat = n - 1
moveNoteWithDegreeType n Sharp = n + 1
moveNoteWithDegreeType n DoubleFlat = n - 2
moveNoteWithDegreeType n DoubleSharp = n + 2

majorIds :: [NoteId]
majorIds = accumulateSteps2ids majorSteps

majorDegreeBase :: [(DegreeId, NoteId)]
majorDegreeBase = [1 .. 7] `zip` majorIds

getDegreeBaseById :: DegreeId -> (DegreeId, NoteId)
getDegreeBaseById degId = majorDegreeBase !! (degId -1)

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x : xs) n = rotate (xs ++ [x]) (n - 1)

extendList :: [a] -> Int -> [a]
extendList lst 0 = lst
extendList lst n = lst ++ extendList lst (n - 1)

extendSteps :: [a] -> [a]
extendSteps steps = extendList steps 7

step2Int :: Step -> Int
step2Int Semitone = 1
step2Int Tone = 2

symbol2id :: NoteSymbol -> NoteId
symbol2id C = 0
symbol2id Cs = 1
symbol2id D = 2
symbol2id Ds = 3
symbol2id E = 4
symbol2id F = 5
symbol2id Fs = 6
symbol2id G = 7
symbol2id Gs = 8
symbol2id A = 9
symbol2id As = 10
symbol2id B = 11

id2symbol :: NoteId -> NoteSymbol
id2symbol noteId =
  case n of
    0 -> C
    1 -> Cs
    2 -> D
    3 -> Ds
    4 -> E
    5 -> F
    6 -> Fs
    7 -> G
    8 -> Gs
    9 -> A
    10 -> As
    11 -> B
    _ -> C
  where
    n = noteId `mod` 12

transposeNote :: Int -> NoteId -> NoteId
transposeNote offset note = offset + note

transposeScale :: Num a => a -> [ScaleNote a] -> [ScaleNote a]
transposeScale z = map (\y -> (fst y + z, snd y))

pitch2id :: PitchProps -> NoteId
pitch2id (note, octave) =
  transposeNote
    ((octave + 1) * 12)
    (symbol2id note)

id2pitch :: NoteId -> Pitch
id2pitch noteId = Pitch (id2symbol noteId, (noteId `div` 12) - 1)

ids2pitches :: [NoteId] -> [Pitch]
ids2pitches = map id2pitch

pitches2ids :: [PitchProps] -> [NoteId]
pitches2ids = map pitch2id

accumulateSteps2ids :: [Step] -> [NoteId]
accumulateSteps2ids = scanl (\x y -> step2Int y + x) 0

pitchDegreesFromTones :: NoteSymbol -> [Degree] -> NoteId -> [ScaleNote Pitch] -> [ScaleNote Pitch]
pitchDegreesFromTones note [] accNoteId accScaleNotes = accScaleNotes
pitchDegreesFromTones note ((Degree x degType degId) : degs) accNoteId accScaleNotes =
  let (_, matchingNoteId) = getDegreeBaseById degId
      -- + 12 to transpose to the 0 octave
      newNoteId = transposeNote (symbol2id note + 12) (moveNoteWithDegreeType matchingNoteId degType)
      updatedScaleNotes = (accScaleNotes ++ [(id2pitch newNoteId, Degree x degType degId)])
   in pitchDegreesFromTones note degs newNoteId updatedScaleNotes

createScale :: ScaleBlueprint a -> NoteSymbol -> PitchedScale a Pitch
createScale (ScaleBlueprint kind degrees) note =
  -- sorting by pitch would be nice here
  let pitchedDegrees = pitchDegreesFromTones note degrees 0 []
   in PitchedScale note kind pitchedDegrees

replaceDegreeAt :: [Degree] -> Degree -> [Degree]
replaceDegreeAt degs (Degree x1 y1 i1) =
  map
    ( \(Degree x y i) ->
        if i == i1 then Degree x1 y1 i1 else Degree x y i
    )
    degs

(-<) :: [Degree] -> Degree -> [Degree]
xs -< x = replaceDegreeAt xs x

(-+) :: [Degree] -> Degree -> [Degree]
xs -+ x = x : xs

(-!) :: [Degree] -> Degree -> [Degree]
xs -! x = filter (x /=) xs

ionianDegs :: [Degree]
ionianDegs = map (Degree Local Reg) [1 .. 7]

aeolianDegs :: [Degree]
aeolianDegs =
  ionianDegs
    -< Degree Local Flat 3
    -< Degree Local Flat 6
    -< Degree Local Flat 7

harmonicMinorDegs :: [Degree]
harmonicMinorDegs =
  aeolianDegs
    -< Degree Local Reg 7

minorPentaDegs :: [Degree]
minorPentaDegs =
  aeolianDegs
    -! Degree Local Reg 2
    -! Degree Local Flat 6

minorBluesDegs :: [Degree]
minorBluesDegs =
  minorPentaDegs
    -+ Degree Foreign Sharp 4

scale :: Scale -> NoteSymbol -> PitchedScale Scale Pitch
scale Major = createScale $ ScaleBlueprint Major ionianDegs
scale Minor = createScale $ ScaleBlueprint Minor aeolianDegs
scale HarmonicMinor = createScale $ ScaleBlueprint HarmonicMinor harmonicMinorDegs
scale PentatonicMinor = createScale $ ScaleBlueprint PentatonicMinor minorPentaDegs
scale MinorBlues = createScale $ ScaleBlueprint MinorBlues minorBluesDegs
scale Ionian = createScale $ ScaleBlueprint Ionian ionianDegs
scale Aeolian = createScale $ ScaleBlueprint Aeolian aeolianDegs
