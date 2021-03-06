module Main where

import Codec.Midi (exportFile)
import Harmony
import Midi

sequence1 :: Sequence a
sequence1 =
  Note 12 (Pitch (A, 4))
    :~: Rest 12
    :~: Note 12 (Pitch (C, 3))
    :~: Rest 12
    :~: Chord 12 [Pitch (C, 2), Pitch (C, 3), Pitch (A, 3), Pitch (G, 4)]
    :~: Rest 12
    :~: Chord 12 [Pitch (C, 3), Pitch (A, 3), Pitch (B, 3)]

track0 :: MidiTrack
track0 = makeTrack (concatMap (`midiNote` 2) (accumulateSteps2ids majorSteps))

track1 :: MidiTrack
track1 = makeTrack (midiNote 60 12)

track2 :: MidiTrack
track2 = makeTrack (generateScaleMidi Minor As)

track3 :: MidiTrack
track3 =
  makeTrack (sequenceToMidi sequence1)

main :: IO ()
main = exportFile "test_track3.mid" (makeMidi [track3])