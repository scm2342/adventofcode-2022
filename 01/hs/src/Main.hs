module Main(main) where

import System.IO
import Data.List (sortBy)

main :: IO ()
main = do
  contents <- hGetContents stdin
  let sorted = sortBy (flip compare) $ map (sum . map (read :: String -> Int)) $ splitWhen null $ lines contents
  print $ head sorted
  print $ sum $ take 3 sorted

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs = case break p xs of
  ([], _ : rs) -> splitWhen p rs
  (pref, rs) -> pref : splitWhen p rs
