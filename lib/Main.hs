module Main where

import Codec.Midi (exportFile)
import Midi
import Harmony

sequence1 :: Sequence a
sequence1 =
  Note 12 (Pitch (A, 4))
    :~: Rest 12
    :~: Note 12 (Pitch (C, 3))
    :~: Rest 12
    :~: Chord 12 (Pitches [(C, 3), (A, 3), (G, 4)])
    :~: Rest 12
    :~: Chord 12 (Pitches [(C, 3), (A, 3), (B, 3)])

track0 :: MidiTrack
track0 = makeTrack (concatMap (`midiNote` 2) (buildScale majorSteps))

track1 :: MidiTrack
track1 = makeTrack (midiNote 60 12)

track2 :: MidiTrack
track2 = makeTrack (midiChord (initMode Aeolian B) 12)

track3 :: MidiTrack
track3 =
  makeTrack (sequenceToMidi sequence1)

main :: IO ()
main = exportFile "test_track3.mid" (makeMidi [track3])