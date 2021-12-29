module Harmony where

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

type Degree = Int

type ScalePitchProps = (NoteSymbol, OctaveNumber, Degree)

data ScaleBlueprint a = ScaleBlueprint a [Step]
  deriving (Show)

data PitchedScale a = PitchedScale NoteSymbol (ScaleBlueprint Mode) [ScalePitchProps]
  deriving (Show)

newtype Pitch = Pitch PitchProps
  deriving (Show)

newtype Pitches = Pitches [PitchProps]
  deriving (Show)

data Sequence a
  = Note Duration Pitch
  | Chord Duration Pitches
  | Rest Duration
  | Sequence a :~: Sequence a
  deriving (Show)

majorSteps :: [Step]
majorSteps = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

rotateSteps :: [Step] -> Int -> [Step]
rotateSteps [] _ = []
rotateSteps steps 0 = steps
rotateSteps (x : xs) n = rotateSteps (xs ++ [x]) (n - 1)

extendList :: [a] -> Int -> [a]
extendList lst 0 = lst
extendList lst n = lst ++ extendList lst (n - 1)

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

pitch2id :: PitchProps -> NoteId
pitch2id (note, octave) =
  transposeNote
    ((octave + 1) * 12)
    (symbol2id note)

id2pitch :: NoteId -> PitchProps
id2pitch noteId = (id2symbol noteId, (noteId `div` 12) - 1)

ids2pitches :: [NoteId] -> [PitchProps]
ids2pitches = map id2pitch

pitches2ids :: [PitchProps] -> [NoteId]
pitches2ids = map pitch2id

buildScale :: [Step] -> [NoteId]
buildScale steps =
  transpose 24 (scanl (\x y -> step2Int y + x) 0 (extendList steps 6))

buildPitchedMode :: NoteId -> NoteSymbol -> [NoteId]
buildPitchedMode n note =
  transpose
    (symbol2id note)
    (buildScale (rotateSteps majorSteps n))

initMode :: Mode -> NoteSymbol -> [NoteId]
initMode Ionian = buildPitchedMode 0
initMode Dorian = buildPitchedMode 1
initMode Phrygian = buildPitchedMode 2
initMode Lydian = buildPitchedMode 3
initMode Mixolodian = buildPitchedMode 4
initMode Aeolian = buildPitchedMode 5
initMode Locrian = buildPitchedMode 6

-- getMainChordsFromScale ::