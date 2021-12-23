module Main where

{-
  pitches (C, 4)
  durations
-}

type Note = String

data Step = Semitone | Tone deriving(Show)

type Pitch = (Note, Integer)

ionianSteps :: [Step]
ionianSteps = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

rotateSteps :: [Step] -> Int -> [Step]
rotateSteps [] _ = []
rotateSteps steps 0 = steps
rotateSteps (x:xs) n = rotateSteps (xs ++ [x]) (n - 1)

step2Int :: Step -> Int
step2Int Semitone = 1
step2Int Tone = 2

accumulateSteps :: [Step] -> [Int]
accumulateSteps = scanl (\x y -> step2Int y + x) 0

(-+-) :: [a] -> [a] -> [a]
val1 -+- val2 = val1 ++ val2

pitch1 :: Pitch
pitch1 = ("A", 3)


main :: IO ()
main = putStrLn "Sup"
