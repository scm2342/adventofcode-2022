module Main(main) where

import System.IO

data RPS = Rock | Paper | Scissors deriving (Show, Read, Eq)
rating :: RPS -> Int
rating Rock = 1
rating Paper = 2
rating Scissors = 3

data Outcome = Win | Draw | Loss deriving (Show, Read, Eq)

winning :: RPS -> RPS -> Outcome
winning a b | a == b = Draw
winning Scissors Paper = Win
winning Paper Rock = Win
winning Rock Scissors = Win
winning _ _ = Loss

chooseMove :: RPS -> Outcome -> RPS
chooseMove a Draw = a
chooseMove Scissors Win = Rock
chooseMove Scissors Loss = Paper
chooseMove Paper Win = Scissors
chooseMove Paper Loss = Rock
chooseMove Rock Win = Paper
chooseMove Rock Loss = Scissors

gradeSucc :: Outcome -> Int
gradeSucc Win = 6
gradeSucc Draw = 3
gradeSucc Loss = 0

grade :: RPS -> Outcome -> Int
grade mine outcome = rating mine + gradeSucc outcome

parseLine :: (String -> a) -> String -> (RPS, a)
parseLine parse str = (parseForm $ ws !! 0, parse $ ws !! 1)
  where
  ws = words str

parseForm "A" = Rock
parseForm "B" = Paper
parseForm "C" = Scissors
parseForm "X" = Rock
parseForm "Y" = Paper
parseForm "Z" = Scissors
parseOutcome "X" = Loss
parseOutcome "Y" = Draw
parseOutcome "Z" = Win

main :: IO ()
main = do
  contents <- hGetContents stdin >>= return . lines
  let moves1 = map (parseLine parseForm) contents
  print $ sum $ map (\(opponent, mine) -> grade mine (winning mine opponent)) moves1
  let moves2 = map (parseLine parseOutcome) contents
  print $ sum $ map (\(opponent, outcome) -> grade (chooseMove opponent outcome) outcome) moves2
