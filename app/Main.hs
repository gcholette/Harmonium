module Main where

import Codec.Midi
  ( FileType (MultiTrack),
    Message (NoteOff, NoteOn, TrackEnd),
    Midi (Midi, fileType, timeDiv, tracks),
    Ticks,
    TimeDiv (TicksPerBeat),
    exportFile,
  )

infixr 5 :~:

type Semitones = Int

type Duration = Rational

type NumericNote = Int

data Mode
  = Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolodian
  | Aeolian
  | Locrian

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

data Note a = Pitch Duration a | Rest Duration

data Sequence a = Note a | Sequence a :~: Sequence a
  deriving (Show)

sequence1 :: Sequence NoteSymbol
sequence1 = Note A :~: Note B :~: Note C

sequenceToMidi :: Sequence NoteSymbol -> Int
sequenceToMidi (Note a1) = note2NumericNote a1
sequenceToMidi (a1 :~: a2) = sequenceToMidi a1

note2NumericNote :: NoteSymbol -> NumericNote
note2NumericNote C = 60
note2NumericNote Cs = 61
note2NumericNote D = 62
note2NumericNote Ds = 63
note2NumericNote E = 64
note2NumericNote F = 65
note2NumericNote Fs = 66
note2NumericNote G = 67
note2NumericNote Gs = 68
note2NumericNote A = 69
note2NumericNote As = 70
note2NumericNote B = 71

step2Int :: Step -> Int
step2Int Semitone = 1
step2Int Tone = 2

ionianSteps :: [Step]
ionianSteps = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

rotateSteps :: [Step] -> Int -> [Step]
rotateSteps [] _ = []
rotateSteps steps 0 = steps
rotateSteps (x : xs) n = rotateSteps (xs ++ [x]) (n - 1)

buildScale :: [Step] -> [Int]
buildScale = scanl (\x y -> step2Int y + x) 0

transpose :: Int -> [Int] -> [Int]
transpose offset = map (+ offset)

midiNote :: NumericNote -> [(Ticks, Message)]
midiNote pitch =
  [ (0, NoteOn 0 pitch 80),
    (24, NoteOn 0 pitch 0)
  ]

track0 :: [(Ticks, Message)]
track0 =
  concatMap midiNote (transpose 0 (buildScale ionianSteps))
    ++ [(0, TrackEnd)]

mainMidi :: Midi
mainMidi =
  Midi
    { fileType = MultiTrack,
      timeDiv = TicksPerBeat 24,
      tracks = [track0]
    }

main :: IO ()
main = exportFile "test.mid" mainMidi
