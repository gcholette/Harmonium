module Main where

import Codec.Midi
  ( FileType (MultiTrack),
    Message (NoteOff, NoteOn, TrackEnd),
    Midi (Midi, fileType, timeDiv, tracks),
    Ticks,
    TimeDiv (TicksPerBeat),
    exportFile,
  )

{-
  pitches (C, 4)
  durations
-}

type Note = String

data Step = Semitone | Tone deriving (Show)

ionianSteps :: [Step]
ionianSteps = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

rotateSteps :: [Step] -> Int -> [Step]
rotateSteps [] _ = []
rotateSteps steps 0 = steps
rotateSteps (x : xs) n = rotateSteps (xs ++ [x]) (n - 1)

step2Int :: Step -> Int
step2Int Semitone = 1
step2Int Tone = 2

accumulateSteps :: [Step] -> [Int]
accumulateSteps = scanl (\x y -> step2Int y + x) 0

raiseNotes :: Int -> [Int] -> [Int]
raiseNotes offset = map (+ offset)

(-+-) :: [Int] -> [Int] -> [Int]
val1 -+- val2 = val1 ++ val2 ++ [1, 2, 3]

midiNote :: Int -> [(Ticks, Message)]
midiNote pitch =
  [ (0, NoteOn 0 pitch 80),
    (24, NoteOn 0 pitch 0)
  ]

track0 :: [(Ticks, Message)]
track0 =
  concatMap midiNote (raiseNotes 60 (accumulateSteps ionianSteps))
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
