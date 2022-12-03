module Main(main) where

import System.IO
import qualified Data.Set as S
import Data.Char

main :: IO ()
main = do
  contents <- hGetContents stdin >>= return . lines
  print $ sum $ map (priority . intersectRucksack . stringToRucksack) contents
  print $ sum $ map (priority . (\[a, b, c] -> intersectUnique3 a b c)) $ group3 contents

data Rucksack = Rucksack String String deriving (Show)

group3 :: [a] -> [[a]]
group3 [] = []
group3 xs = a : group3 b
  where
  (a, b) = splitAt 3 xs
intersectUnique3 :: Ord a => [a] -> [a] -> [a] -> a
intersectUnique3 a b c = S.elemAt 0 (S.fromList a `S.intersection` S.fromList b `S.intersection` S.fromList c)

intersectRucksack :: Rucksack -> Char
intersectRucksack (Rucksack a b) = S.elemAt 0 (S.fromList a `S.intersection` S.fromList b)
stringToRucksack :: String -> Rucksack
stringToRucksack str = uncurry Rucksack $ splitAt (length str `div` 2) str

priority :: Char -> Int
priority c
  | isUpper c = ord c - 64 + 26
  | otherwise = ord c - 96
