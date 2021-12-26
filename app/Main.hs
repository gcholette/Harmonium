module Main where

import Codec.Midi
  ( FileType (MultiTrack),
    Message (NoteOff, NoteOn, TrackEnd),
    Midi (Midi, fileType, timeDiv, tracks),
    Ticks,
    TimeDiv (TicksPerBeat),
    exportFile
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

data NoteProperties a = Pitch Duration a | Rest Duration
  deriving (Show)

data Sequence a = Note (NoteProperties a) | Sequence a :~: Sequence a
  deriving (Show)

sequence1 :: Sequence NoteSymbol
sequence1 = Note (Pitch 30 A) :~: Note (Rest 30)

sequenceToMidi :: Sequence NoteSymbol -> Int
sequenceToMidi (Note (Pitch _ a1)) = note2NumericNote a1
sequenceToMidi (Note (Rest a1)) = 0
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

midiNote :: NumericNote -> Ticks -> [(Ticks, Message)]
midiNote pitch ticks =
  [
    (0, NoteOn 0 pitch 80),
    (ticks, NoteOn 0 pitch 0)
  ]

midiChord :: [NumericNote] -> Ticks -> [(Ticks, Message)]
midiChord [] ticks = []
midiChord (x : xs) ticks =
  map (\note -> (0, NoteOn 0 note 80)) (x : xs)
    ++ [(ticks, NoteOn 0 x 0)]
    ++ map (\note -> (0, NoteOn 0 note 0)) xs

track0 :: [(Ticks, Message)]
track0 =
  concatMap (`midiNote` 2) (transpose 0 (buildScale ionianSteps))
    ++ [(0, TrackEnd)]

track1 :: [(Ticks, Message)]
track1 =
  midiNote 60 12
    ++ [(0, TrackEnd)]

track2 :: [(Ticks, Message)]
track2 =
  midiChord (transpose 0 (buildScale ionianSteps)) 12
    ++ [(0, TrackEnd)]

mainMidi :: Midi
mainMidi =
  Midi
    { fileType = MultiTrack,
      timeDiv = TicksPerBeat 12,
      tracks = [track2]
    }

main :: IO ()
main = exportFile "test.mid" mainMidi
