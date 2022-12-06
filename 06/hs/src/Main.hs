{-# LANGUAGE FlexibleContexts,RecordWildCards #-}
module Main(main) where

import System.IO
import Control.Monad
import qualified Data.List as L
import qualified Data.Set as S

main :: IO ()
main = do
  content <- hGetContents stdin
  forM_ (lines content) $ print . head . markers 4
  forM_ (lines content) $ print . head . markers 14

markers :: Int -> String -> [Int]
markers n str = map (+1) $ L.findIndices id $ map (\xs -> S.size (S.fromList xs) == n) zipped
  where
  zipped = zipN n str

zipN :: Int -> String -> [String]
zipN n = reverse . zipNReverse n . reverse
  where
  zipNReverse _ [] = []
  zipNReverse m str@(_:tl) = reverse (take m str) : zipNReverse m tl
