{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Day13 where

import           Data.List (group, sort, foldl', find)
import           Data.List.Extra (split, chunksOf)
import qualified Data.Set as S

data Pos = Pos { x :: Int, y :: Int } deriving (Eq, Show, Ord)

type Board = [Pos]

data Command = FoldX Int | FoldY Int deriving (Eq, Show, Ord)

-- first is x -->  from left to right
-- second is y --> from top  to bottom

foldXPos :: Int -> Pos -> Pos
foldXPos fx Pos { .. } = Pos {
    x = if x < fx then x else 2*fx - x,
    y = y
}

foldYPos :: Int -> Pos -> Pos
foldYPos fy Pos { .. } = Pos {
    x = x,
    y = if y < fy then y else 2*fy - y
}

foldBoard :: Command -> Board -> Board
foldBoard (FoldX n) = (foldXPos n <$>)
foldBoard (FoldY n) = (foldYPos n <$>)

boardSize :: Board -> Int
boardSize = length . group . sort

parsePos :: String -> Pos
parsePos line =
  let [xp, yp] = split (== ',') line
  in Pos { x = read $ xp, y = read yp }

parseBoard :: [String] -> [Pos]
parseBoard = (parsePos <$>)

parseCommand :: String -> Command
parseCommand line =
  case (line !! 11) of
    'x' -> FoldX $ read (drop 13 line)
    'y' -> FoldY $ read (drop 13 line)

part1 :: Board -> Command -> Int
part1 b c = boardSize $ foldBoard c b

printBoard :: Int -> Int -> Board -> [String]
printBoard xDim yDim board =
  let points = S.fromList board
      mapLine y = (\x ->
              if Pos { x = x, y = y } `S.member` points then 'X' else '.') <$> [0 .. xDim - 1]
  in mapLine <$> [0 .. yDim - 1]

part2 :: Board -> [Command] -> [String]
part2 b cs =
  let finalBoard = foldl' (flip foldBoard) b cs
      in printBoard 40 6 finalBoard

main :: IO ()
main = do
  ls <- lines <$> getContents
  let [boardData, foldingInstructions] = split (== "") ls
  let board = parseBoard boardData
  let commands = parseCommand <$> foldingInstructions
  print $ part1 board $ head commands
  mapM_ print $ part2 board commands
