{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day11 where

import           Control.Monad (mapM_)
import qualified Data.Vector as V
import qualified Data.Map as M
import           Data.Vector ((!), (//))
import           Data.Char (digitToInt, intToDigit)
import           Data.List (sort, group)
import           Data.List.Extra (chunksOf)


-- octopus flashing simulation
-- 1. apply (+1) on every cell
-- 2. apply flashing, map (+1) over neighbors but not on `0`
--    as `0` indicates it already flashed during that period
-- 3. count number of `0`, record and proceed from 1.

type Row = Int
type Col = Int

-- row-major, i.e. 10*row + col (for 10 cols)
data OctopusBoard = OctopusBoard {
    rows :: Row,
    cols :: Col,
    cells :: V.Vector Int } deriving (Show)

data Pos = Pos {
    row :: Row,
    col :: Col } deriving (Show, Eq, Ord)

parseBoard :: [String] -> OctopusBoard
parseBoard ls =
  let rows  = length ls
      cols  = length $ head ls
      cells = V.fromList $ concat $ (digitToInt <$>) <$> ls
  in OctopusBoard { .. }

positions :: OctopusBoard -> [Pos]
positions OctopusBoard { .. } = uncurry Pos <$> ([0 .. rows-1] >>= \x -> zip (repeat x) [0 .. cols-1])

energyAt :: OctopusBoard  -> Pos -> Int
energyAt board pos = cells board ! index board pos

index :: OctopusBoard -> Pos -> Int
index OctopusBoard { .. } Pos { .. }  = row * cols + col

validPos :: OctopusBoard -> Pos -> Bool
validPos OctopusBoard { .. } Pos { .. }
  = row >= 0 && col >= 0 && row < rows && col < cols

move :: Pos -> (Int, Int) -> Pos
move Pos { .. } (r, c) = Pos { row = row + r, col = col + c }

-- produce a list of all valid neighbors
-- needs the board to know the boundaries etc.
neighbors :: OctopusBoard -> Pos -> [Pos]
neighbors board pos =
    let ns = move pos <$> [
          (-1, -1), (-1, 0), (-1, 1),
          (0, -1), (0, 1),
          (1, -1), (1, 0), (1, 1) ]
    in validPos board `filter` ns

-- octopus that flashed already is zero, but it doesn't
-- gain additional energy from neighbors flashing
flashMod :: Int -> Int -> Int
flashMod 0 _ = 0
flashMod n k = min 10 $ n + k -- apply k flashes in one go

computeFlashingPositions :: OctopusBoard -> [Pos]
computeFlashingPositions board = filter (\p -> energyAt board p >= 10) $ positions board

flash :: OctopusBoard -> OctopusBoard
flash board@OctopusBoard { .. } =
  let flashing = computeFlashingPositions board
      -- note that this can contain positions multiple times, for multiple flashing neighbors
      ns = sort $ flashing >>= neighbors board
      additions = map (\ps -> (index board (head ps),
                  flashMod (energyAt board (head ps)) (length ps))) $ group ns
      resets = ((\p -> (index board p, 0)) <$> flashing)
  in if null flashing  -- no new flashings this round
       then board
       else flash board { cells = cells // additions // resets }

advanceBoard :: OctopusBoard -> OctopusBoard
advanceBoard board =
  let nextInitialBoard = board { cells = (+1) `V.map` cells board }
  in flash nextInitialBoard

printBoard :: OctopusBoard -> IO ()
printBoard OctopusBoard { .. } = mapM_ print $ chunksOf cols $ intToDigit <$> V.toList cells

part1 :: OctopusBoard -> Int
part1 board =
  let boards = take 101 $ iterate advanceBoard board
  in sum $ map (V.length . V.filter (==0) . cells) boards

main :: IO ()
main = do
  -- printBoard $ iterate advanceBoard eb !! 100
  print $ part1 $ parseBoard exampleBoard
  print $ part1 $ parseBoard inputBoard


  -- advanceBoard $ advanceBoard eb

inputBoard, exampleBoard :: [String]
exampleBoard = [
    "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526"
  ]

inputBoard = [
    "6227618536",
    "2368158384",
    "5385414113",
    "4556757523",
    "6746486724",
    "4881323884",
    "4648263744",
    "4871332872",
    "4724128228",
    "4316512167"
  ]
