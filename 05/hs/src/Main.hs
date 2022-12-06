{-# LANGUAGE FlexibleContexts,RecordWildCards #-}
module Main(main) where

import System.IO
import System.Exit
import Text.Parsec
import Data.List (transpose)
import Data.Maybe (catMaybes)
import qualified Data.Array as A

main :: IO ()
main = do
  input <- getPuzzleInput
  print $ task1 input
  print $ task2 input

task1 :: Input -> String
task1 (Input {..}) = map head $ A.elems $ foldl (move9000and9001 False) stacks moves
task2 :: Input -> String
task2 (Input {..}) = map head $ A.elems $ foldl (move9000and9001 True) stacks moves

parseElem :: Stream s m Char => ParsecT s u m (Maybe Char)
parseElem = (do
  _ <- char '['
  c <- anyChar
  _ <- char ']'
  return $ Just c) <|> (do
  _ <- count 3 (char ' ')
  return Nothing)
parseElems :: Stream s m Char => ParsecT s u m [Maybe Char]
parseElems = parseElem `sepBy` char ' '
parseElemsLines :: Stream s m Char => ParsecT s u m [[Maybe Char]]
parseElemsLines = try parseElems `sepEndBy` endOfLine
parseSeperatorLines :: Stream s m Char => ParsecT s u m ()
parseSeperatorLines = do
  _ <- (char ' ' >> digit >> char ' ') `sepBy` char ' '
  _ <- count 2 endOfLine
  return ()
data Move = Move {
    amount :: Int
  , from :: Int
  , to :: Int
  } deriving Show
parseMove :: Stream s m Char => ParsecT s u m Move
parseMove = do
  _ <- string "move "
  n <- many1 digit
  _ <- string " from "
  s <- many1 digit
  _ <- string " to "
  t <- many1 digit
  return $ Move (read n) (read  s) (read t)
parseMoves :: Stream s m Char => ParsecT s u m [Move]
parseMoves = parseMove `sepEndBy` endOfLine
type Stack = String
data Input = Input {
    stacks :: A.Array Int Stack
  , moves :: [Move]
  } deriving Show
parseInput :: Stream s m Char => ParsecT s u m Input
parseInput = do
  s <- parseElemsLines
  parseSeperatorLines
  m <- parseMoves
  _ <- eof
  let sa = (\ls -> A.listArray (1, length ls) ls) $ map catMaybes $ transpose s
  return $ Input sa m

getPuzzleInput :: IO Input
getPuzzleInput = do
  content <- hGetContents stdin
  either (\e -> hPutStrLn stderr (show e) >> exitFailure) return $ parse parseInput "stdin" content

move9000and9001 :: Bool -> A.Array Int Stack -> Move -> A.Array Int Stack
move9000and9001 is9001 arr (Move {..}) = arr A.// [(from, nfrom), (to, nto)]
  where
  sfrom = arr A.! from
  sto = arr A.! to
  (tomove, nfrom) = splitAt amount sfrom
  nto = (if is9001 then tomove else reverse tomove) ++ sto
