module Main where

{-

  pitches (C, 4)
  durations

-}
data Note = A | As

type Pitch = (Note, Integer)

(-+-) :: [a] -> [a] -> [a]
val1 -+- val2 = val1 ++ val2

pitch1 :: Pitch
pitch1 = (A, 3)


main :: IO ()
main = putStrLn "Hello, Haskell!"
