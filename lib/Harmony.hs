module Harmony where

-- Functional harmony, classic theory

import Data.Function ((&))

infixr 5 :~:

type Semitones = Int

type Duration = Int

type NoteId = Int

type OctaveNumber = Int

data Mode
  = Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolodian
  | Aeolian
  | Locrian
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

-- might not use pitches in logic except for "manual" input
-- Step -> NoteId -> Pitch -> NoteId ~> Midi
newtype Pitch = Pitch PitchProps
  deriving (Show)

newtype Pitches = Pitches [PitchProps]
  deriving (Show)

type DegreeId = Int

data DegreeType = Reg | Sharp | Flat | DoubleSharp | DoubleFlat
  deriving (Show)

data Locality = Foreign | Local
  deriving (Show)

data Degree = Degree Locality DegreeType DegreeId
  deriving (Show)

type ScaleNote a = (a, Degree)

data ScaleBlueprint a = ScaleBlueprint a [Degree]
  deriving (Show)

data PitchedScale a = PitchedScale NoteSymbol a [ScaleNote NoteId]
  deriving (Show)

a1 :: ScaleBlueprint Mode
a1 =
  ScaleBlueprint
    Aeolian
    [ Degree Local Reg 1,
      Degree Local Reg 2,
      Degree Local Flat 3,
      Degree Local Reg 4,
      Degree Foreign Sharp 4,
      Degree Local Reg 5,
      Degree Local Flat 6,
      Degree Local Flat 7
    ]

a2 :: ScaleBlueprint Mode -> [Int]
a2 (ScaleBlueprint Aeolian _) = [1]
a2 (ScaleBlueprint _ ((Degree l t i) : xs)) = [1]
a2 (ScaleBlueprint _ _) = [1]

data Sequence a
  = Note Duration Pitch
  | Chord Duration Pitches
  | Rest Duration
  | Sequence a :~: Sequence a
  deriving (Show)

majorSteps :: [Step]
majorSteps = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

diminishedSteps :: [Step]
diminishedSteps = [Tone, Semitone, Tone, Semitone, Tone, Semitone, Tone]

rotateSteps :: [Step] -> Int -> [Step]
rotateSteps [] _ = []
rotateSteps steps 0 = steps
rotateSteps (x : xs) n = rotateSteps (xs ++ [x]) (n - 1)

--addStepDegrees :: [Step] -> [ScaleNote Step]
--addStepDegrees steps = zip steps (map Reg [1 .. (length steps)])

--applyPitchToScaleBlueprint :: [StepBlueprint] -> NoteSymbol -> [ScalePitchProps]
--applyPitchToScaleBlueprint steps note =
--  let (a,b) = unzip steps
--  in zip (map id2pitch (accumulateSteps2ids steps)) b

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

transpose :: Int -> [NoteId] -> [NoteId]
transpose offset = map (+ offset)

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

buildPitchedMode :: NoteId -> NoteSymbol -> [NoteId]
buildPitchedMode n note =
  transpose
    (symbol2id note)
    (accumulateSteps2ids $ rotateSteps (extendSteps majorSteps) n)

initMode :: Mode -> NoteSymbol -> [NoteId]
initMode Ionian = buildPitchedMode 0
initMode Dorian = buildPitchedMode 1
initMode Phrygian = buildPitchedMode 2
initMode Lydian = buildPitchedMode 3
initMode Mixolodian = buildPitchedMode 4
initMode Aeolian = buildPitchedMode 5
initMode Locrian = buildPitchedMode 6

-- getMainChordsFromScale ::