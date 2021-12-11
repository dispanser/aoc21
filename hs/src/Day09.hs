{-# LANGUAGE RecordWildCards #-}
module Day09 where

import qualified Data.Vector as V
import           Data.Vector ( (!) )
import Data.Char (digitToInt)

data Grid = Grid {
    width  :: Int,
    height :: Int,
    grid :: V.Vector (V.Vector Int)
} deriving (Eq, Show)

data Point = Point { row :: Int, col :: Int } deriving (Show, Eq)

data Direction = Up | Down | Left | Right

move :: Point -> Direction -> Point
move p@Point { .. } Up = p { row = row - 1 }
move p@Point { .. } Down = p { row = row + 1 }
move p@Point { .. } Left = p { col = col - 1 }
move p@Point { .. } Right = p { col = col + 1 }

heightAt :: Grid -> Point -> Int
heightAt Grid {..} Point { .. }
    | row < 0 || col < 0            = 10 -- just heigher than on the map
    | row >= height || col >= width = 10
    | otherwise = grid ! row ! col


isMinPoint :: Grid -> Point-> Bool
isMinPoint g p =
  let h = heightAt g p
  in all (> h) $ heightAt g . move p <$> [Left, Right, Up, Down]

parseGrid :: [String] -> Grid
parseGrid ls = Grid {
    width  = V.length $ V.head grid,
    height = V.length grid,
    grid = grid
  }
    where grid = V.fromList $ V.fromList <$> ((digitToInt <$>) <$> ls)

-- [1, 2, 3] >>= \i -> Prelude.zip (repeat i) [7, 8, 9]
part1 :: Grid -> Int
part1 g@Grid {..} =
    let coords = uncurry Point <$> ([0 .. height-1] >>= \x -> Prelude.zip (repeat x) [0 .. width-1])
        minPoints  = filter (isMinPoint g) coords
        cellValues = heightAt g <$> minPoints
    in sum $ (+1) <$> cellValues

part2 :: Grid -> Int
part2 g@Grid { .. } = 0

main :: IO ()
main = do
    ls <- lines <$> getContents
    print . part1 $ parseGrid ls

g0 = Grid {width = 10, height = 5, grid = V.fromList [V.fromList [2,1,9,9,9,4,3,2,1,0],V.fromList [3,9,8,7,8,9,4,9,2,1],V.fromList [9,8,5,6,7,8,9,8,9,2],V.fromList [8,7,6,7,8,9,6,7,8,9],V.fromList [9,8,9,9,9,6,5,6,7,8]]}

