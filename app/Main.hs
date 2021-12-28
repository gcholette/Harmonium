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

type Duration = Int

type NoteId = Int

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

data ChordProps = Pitches Duration [PitchProps]
  deriving (Show)

data Sequence a
  = Note NoteProps
  | Chord ChordProps
  | Sequence a :~: Sequence a
  deriving (Show)

majorSteps :: [Step]
majorSteps = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

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

initMode :: Mode -> NoteSymbol -> [NoteId]
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

buildPitchedMode :: NoteId -> NoteSymbol -> [NoteId]
buildPitchedMode n note =
  transpose
    (symbol2id note)
    (buildScale (rotateSteps majorSteps n))

transpose :: Int -> [NoteId] -> [NoteId]
transpose offset = map (+ offset)

transposeNote :: Int -> NoteId -> NoteId
transposeNote offset note = offset + note

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

-- need to look into making rests in midi
midiNote :: NoteId -> Ticks -> Track
midiNote (-1) ticks = [(0, NoteOff 0 (-200) 0), (ticks, NoteOff 0 (-200) 0)]
midiNote noteId ticks = [(0, NoteOn 0 noteId 80), (ticks, NoteOn 0 noteId 0)]

midiChord :: [NoteId] -> Ticks -> Track
midiChord [] ticks = []
midiChord (x : xs) ticks =
  map (\note -> (0, NoteOn 0 note 80)) (x : xs)
    ++ [(ticks, NoteOn 0 x 0)]
    ++ map (\note -> (0, NoteOn 0 note 0)) xs

sequence1 :: Sequence a
sequence1 =
  Note (Pitch 12 (A, 4))
    :~: Note (Rest 12)
    :~: Note (Pitch 12 (C, 3))
    :~: Note (Rest 12)
    :~: Chord (Pitches 12 [(C, 3), (A, 3), (G, 4)])
    :~: Note (Rest 12)
    :~: Chord (Pitches 12 [(C, 3), (A, 3), (B, 3)])

pitch2id :: PitchProps -> NoteId
pitch2id (note, octave) =
  transposeNote
    ((octave + 1) * 12)
    (symbol2id note)

sequenceToMidi :: Sequence NoteProps -> Track
sequenceToMidi (a1 :~: a2) = sequenceToMidi a1 ++ sequenceToMidi a2
sequenceToMidi (Note (Rest dur)) = midiNote (-1) dur
sequenceToMidi (Note (Pitch dur pitch)) = midiNote (pitch2id pitch) dur
sequenceToMidi (Chord (Pitches dur pitches)) = midiChord (map pitch2id pitches) dur

track0 :: Track
track0 = makeTrack (concatMap (`midiNote` 2) (buildScale majorSteps))

track1 :: Track
track1 = makeTrack (midiNote 60 12)

track2 :: Track
track2 = makeTrack (midiChord (initMode Aeolian B) 12)

track3 :: Track
track3 =
  makeTrack (sequenceToMidi sequence1)

makeTrack :: Track -> Track
makeTrack t = t ++ [(0, TrackEnd)]

makeMidi :: [Track] -> Midi
makeMidi tracks =
  Midi
    { fileType = MultiTrack,
      timeDiv = TicksPerBeat 12,
      tracks = tracks
    }

generateModeMidi :: Mode -> NoteSymbol -> Midi
generateModeMidi mode tonality =
  makeMidi [midiChord (initMode mode tonality) 12 ++ [(0, TrackEnd)]]

exportModeToFile :: Mode -> NoteSymbol -> String -> IO ()
exportModeToFile mode tonality filename =
  exportFile filename (generateModeMidi mode tonality)

main :: IO ()
--main = exportModeToFile Aeolian B "b_minor.mid"
main = exportFile "test_track3.mid" (makeMidi [track3])
