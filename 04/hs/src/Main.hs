{-# LANGUAGE FlexibleContexts #-}
module Main(main) where

import System.IO
import System.Exit
import Text.Parsec

main :: IO ()
main = do
  input <- getIvalInput
  print $ length $ filter id $ map (uncurry inclusion) input
  print $ length $ filter id $ map (uncurry overlap) input

data Interval = Interval Int Int deriving (Show, Eq)

parseInt :: Stream s m Char => ParsecT s u m Int
parseInt = many1 digit >>= return . read
parseInterval :: Stream s m Char => ParsecT s u m Interval
parseInterval = do
  l <- parseInt
  _ <- char '-'
  h <- parseInt
  return $ Interval l h
parseTwoIntervals :: Stream s m Char => ParsecT s u m (Interval, Interval)
parseTwoIntervals = do
  a <- parseInterval
  _ <- char ','
  b <- parseInterval
  return (a, b)
parseIntervalLines :: Stream s m Char => ParsecT s u m [(Interval, Interval)]
parseIntervalLines = parseTwoIntervals `sepEndBy` endOfLine
getIvalInput :: IO [(Interval, Interval)]
getIvalInput = do
  str <- hGetContents stdin
  either (const $ hPutStrLn stderr "failed to parse" >> exitFailure) return $ parse parseIntervalLines "stdin" str

inclusion :: Interval -> Interval -> Bool
inclusion (Interval al ah) (Interval bl bh) = case (compare al bl, compare ah bh) of
  (LT, LT) -> False
  (GT, GT) -> False
  _ -> True
overlap :: Interval -> Interval -> Bool
overlap a@(Interval al ah) b = inclusion a b || inclusion (Interval al al) b || inclusion (Interval ah ah) b
