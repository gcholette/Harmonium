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

type OctaveNumber = Int

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

type PitchProps = (NoteSymbol, OctaveNumber)

data NoteProps = Pitch Duration PitchProps | Rest Duration
  deriving (Show)

data Sequence a
  = Note a
  | Sequence a :~: Sequence a
  deriving (Show)

note2Num :: NoteSymbol -> NumericNote
note2Num C = 0
note2Num Cs = 1
note2Num D = 2
note2Num Ds = 3
note2Num E = 4
note2Num F = 5
note2Num Fs = 6
note2Num G = 7
note2Num Gs = 8
note2Num A = 9
note2Num As = 10
note2Num B = 11

step2Int :: Step -> Int
step2Int Semitone = 1
step2Int Tone = 2

transpose :: Int -> [NumericNote] -> [NumericNote]
transpose offset = map (+ offset)

transposeNote :: NumericNote -> Int -> NumericNote
transposeNote note offset = offset + note

ionianSteps :: [Step]
ionianSteps = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

rotateSteps :: [Step] -> Int -> [Step]
rotateSteps [] _ = []
rotateSteps steps 0 = steps
rotateSteps (x : xs) n = rotateSteps (xs ++ [x]) (n - 1)

extendList :: [a] -> [a]
extendList lst = lst ++ lst ++ lst ++ lst

buildScale :: [Step] -> [Int]
buildScale steps = transpose 24 (scanl (\x y -> step2Int y + x) 0 (extendList steps))

ionianScale :: [NumericNote]
ionianScale = transpose 24 (buildScale ionianSteps)

mode2numeric :: Mode -> [NumericNote]
mode2numeric Ionian = buildScale (rotateSteps ionianSteps 0)
mode2numeric Dorian = buildScale (rotateSteps ionianSteps 1)
mode2numeric Phrygian = buildScale (rotateSteps ionianSteps 2)
mode2numeric Lydian = buildScale (rotateSteps ionianSteps 3)
mode2numeric Mixolodian = buildScale (rotateSteps ionianSteps 4)
mode2numeric Aeolian = buildScale (rotateSteps ionianSteps 5)
mode2numeric Locrian = buildScale (rotateSteps ionianSteps 6)

midiNote :: NumericNote -> Ticks -> [(Ticks, Message)]
midiNote pitch ticks =
  [ (0, NoteOn 0 pitch 80),
    (ticks, NoteOn 0 pitch 0)
  ]

midiChord :: [NumericNote] -> Ticks -> [(Ticks, Message)]
midiChord [] ticks = []
midiChord (x : xs) ticks =
  map (\note -> (0, NoteOn 0 note 80)) (x : xs)
    ++ [(ticks, NoteOn 0 x 0)]
    ++ map (\note -> (0, NoteOn 0 note 0)) xs

sequence1 :: Sequence NoteProps
sequence1 = Note (Pitch 30 (A, 4)) :~: Note (Rest 30)

sequenceToMidi :: Sequence NoteProps -> [Int]
sequenceToMidi (Note (Pitch _ (note, octave))) = [transposeNote (note2Num note) ((octave + 1) * 12)]
sequenceToMidi (Note (Rest a1)) = [0]
sequenceToMidi (a1 :~: a2) = sequenceToMidi a1 ++ sequenceToMidi a2

track0 :: [(Ticks, Message)]
track0 =
  concatMap (`midiNote` 2) (buildScale ionianSteps)
    ++ [(0, TrackEnd)]

track1 :: [(Ticks, Message)]
track1 =
  midiNote 60 12
    ++ [(0, TrackEnd)]

track2 :: [(Ticks, Message)]
track2 =
  midiChord (buildScale ionianSteps) 12
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
