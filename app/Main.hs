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

type Track = [(Ticks, Message)]

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

ionianSteps :: [Step]
ionianSteps = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

note2num :: NoteSymbol -> NumericNote
note2num C = 0
note2num Cs = 1
note2num D = 2
note2num Ds = 3
note2num E = 4
note2num F = 5
note2num Fs = 6
note2num G = 7
note2num Gs = 8
note2num A = 9
note2num As = 10
note2num B = 11

initMode :: Mode -> NoteSymbol -> [NumericNote]
initMode Ionian = buildPitchedMode 0
initMode Dorian = buildPitchedMode 1
initMode Phrygian = buildPitchedMode 2
initMode Lydian = buildPitchedMode 3
initMode Mixolodian = buildPitchedMode 4
initMode Aeolian = buildPitchedMode 5
initMode Locrian = buildPitchedMode 6

step2Int :: Step -> Int
step2Int Semitone = 1
step2Int Tone = 2

buildPitchedMode :: NumericNote -> NoteSymbol -> [NumericNote]
buildPitchedMode n note =
  transpose
    (note2num note)
    (buildScale (rotateSteps ionianSteps n))

transpose :: Int -> [NumericNote] -> [NumericNote]
transpose offset = map (+ offset)

transposeNote :: NumericNote -> Int -> NumericNote
transposeNote note offset = offset + note

rotateSteps :: [Step] -> Int -> [Step]
rotateSteps [] _ = []
rotateSteps steps 0 = steps
rotateSteps (x : xs) n = rotateSteps (xs ++ [x]) (n - 1)

extendList :: [a] -> Int -> [a]
extendList lst 0 = lst
extendList lst n = lst ++ extendList lst (n - 1)

buildScale :: [Step] -> [Int]
buildScale steps =
  transpose 24 (scanl (\x y -> step2Int y + x) 0 (extendList steps 6))

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
sequenceToMidi (Note (Pitch _ (note, octave))) = [transposeNote (note2num note) ((octave + 1) * 12)]
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
  midiChord (initMode Aeolian B) 12
    ++ [(0, TrackEnd)]

mainMidi :: Midi
mainMidi =
  Midi
    { fileType = MultiTrack,
      timeDiv = TicksPerBeat 12,
      tracks = [track2]
    }

makeMidi :: [(Ticks, Message)] -> Midi
makeMidi track =
  Midi
    { fileType = MultiTrack,
      timeDiv = TicksPerBeat 12,
      tracks = [track]
    }

generateModeMidi :: Mode -> NoteSymbol -> Midi
generateModeMidi mode tonality =
  makeMidi (midiChord (initMode mode tonality) 12 ++ [(0, TrackEnd)])

exportModeToFile :: Mode -> NoteSymbol -> String -> IO ()
exportModeToFile mode tonality filename =
  exportFile filename (generateModeMidi mode tonality)

main :: IO ()
main = exportModeToFile Aeolian B "b_minor.mid"
