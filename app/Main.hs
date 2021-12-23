module Main where

{-

  pitches (C, 4)
  durations

-}
type Note = String

data Step = Semitone | Tone deriving(Show)
type Steps = [Step]

type Pitch = (Note, Integer)

ionianSteps :: Steps
ionianSteps = [Tone, Tone, Semitone, Tone, Tone, Tone, Semitone]

rotateSteps :: Steps -> Int -> Steps
rotateSteps [] _ = []
rotateSteps steps 0 = steps
rotateSteps (x:xs) n = rotateSteps (xs ++ [x]) (n - 1)

(-+-) :: [a] -> [a] -> [a]
val1 -+- val2 = val1 ++ val2

pitch1 :: Pitch
pitch1 = ("A", 3)


main :: IO ()
main = putStrLn "Sup"
