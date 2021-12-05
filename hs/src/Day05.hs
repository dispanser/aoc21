{-# LANGUAGE RecordWildCards #-}

module Day05 where

import Control.Monad (forM_, join)
import Text.Parsec (char, many1, digit, string, spaces, parse)
import Text.Parsec.String (Parser)
import Data.Either (rights)
import Data.List (sort, group)

data Point = Point {
    row:: Int,
    col :: Int } deriving (Show, Eq, Ord)

data Vent = Vent {
    start :: Point,
    end   :: Point } deriving (Show, Eq, Ord)

readPoint :: Parser Point
readPoint = spaces *> do
  row <- numberParser
  _ <- char ','
  col <- numberParser
  return Point { row = row, col = col }

readVent :: Parser Vent
readVent = spaces *> do
  p1 <- readPoint
  _  <- spaces *> string "->"
  p2 <- readPoint
  return Vent { start = min p1 p2, end = max p1 p2 }

numberParser :: Parser Int
numberParser = read <$> many1 digit

coveredPoints :: Vent -> [Point]
coveredPoints Vent { .. } =
  let rows = [row start .. row end]
      cols = [col start .. col end]
      steps = max (length rows) (length cols)
  in  uncurry Point <$> take steps (cycle rows `zip` cycle cols)

horizOrVertical :: Vent -> Bool
horizOrVertical Vent { .. } = row start == row end || col start == col end

part1 :: [Vent] -> Int
part1 vents =
    let relevantVents = filter horizOrVertical vents
        points = sort (coveredPoints =<< relevantVents)
    in length $ filter (\ps -> length ps > 1) $ group points

main :: IO ()
main = do
  ls <- lines <$> getContents
  let vents = rights $ parse readVent "" <$> ls
  print $ part1 vents

testInput :: [ String ]
testInput = [
    -- "3,4 -> 3,4",
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2" ]

