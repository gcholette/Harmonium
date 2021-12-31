module Midi where

import Codec.Midi
  ( FileType (MultiTrack),
    Message (NoteOff, NoteOn, TrackEnd),
    Midi (..),
    Ticks,
    TimeDiv (TicksPerBeat),
    exportFile,
  )
import Harmony
  ( Mode,
    NoteId,
    NoteSymbol,
    Pitch (Pitch),
    Pitches (Pitches),
    Sequence (..),
    initModeOld,
    pitch2id,
  )

type MidiTrack = [(Ticks, Message)]

makeTrack :: MidiTrack -> MidiTrack
makeTrack t = t ++ [(0, TrackEnd)]

makeMidi :: [MidiTrack] -> Midi
makeMidi tracks =
  Midi
    { fileType = MultiTrack,
      timeDiv = TicksPerBeat 12,
      tracks = tracks
    }

generateModeMidi :: Mode -> NoteSymbol -> Midi
generateModeMidi mode tonality =
  makeMidi [midiChord (initModeOld mode tonality) 12 ++ [(0, TrackEnd)]]

exportModeToFile :: Mode -> NoteSymbol -> String -> IO ()
exportModeToFile mode tonality filename =
  exportFile filename (generateModeMidi mode tonality)

-- need to look into making rests in midi
midiNote :: NoteId -> Ticks -> MidiTrack
midiNote (-1) ticks = [(0, NoteOff 0 (-200) 0), (ticks, NoteOff 0 (-200) 0)]
midiNote noteId ticks = [(0, NoteOn 0 noteId 80), (ticks, NoteOn 0 noteId 0)]

midiChord :: [NoteId] -> Ticks -> MidiTrack
midiChord [] ticks = []
midiChord (x : xs) ticks =
  map (\note -> (0, NoteOn 0 note 80)) (x : xs)
    ++ [(ticks, NoteOn 0 x 0)]
    ++ map (\note -> (0, NoteOn 0 note 0)) xs

sequenceToMidi :: Sequence a -> MidiTrack
sequenceToMidi (a1 :~: a2) = sequenceToMidi a1 ++ sequenceToMidi a2
sequenceToMidi (Rest dur) = midiNote (-1) dur
sequenceToMidi (Note dur (Pitch pitch)) = midiNote (pitch2id pitch) dur
sequenceToMidi (Chord dur (Pitches pitches)) = midiChord (map pitch2id pitches) dur
