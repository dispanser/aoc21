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

heightAt :: Grid -> Int -> Int -> Int
heightAt Grid {..} x y
    | x < 0 || y < 0            = 10 -- just heigher than on the map
    | x >= height || y >= width = 10
    | otherwise = grid ! x ! y

isMinPoint :: Grid -> Int -> Int -> Bool
isMinPoint g x y =
  let h = heightAt g x y
  in
    heightAt g (x-1) y > h &&
    heightAt g (x+1) y > h &&
    heightAt g x (y-1) > h &&
    heightAt g x (y+1) > h

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
    let coords = [0 .. height-1] >>= \x -> Prelude.zip (repeat x) [0 .. width-1]
        minPoints  = filter (uncurry (isMinPoint g)) coords
        cellValues = uncurry (heightAt g) <$> minPoints
    in sum $ (+1) <$> cellValues

main :: IO ()
main = do
    ls <- lines <$> getContents
    print . part1 $ parseGrid ls

g0 = Grid {width = 10, height = 5, grid = V.fromList [V.fromList [2,1,9,9,9,4,3,2,1,0],V.fromList [3,9,8,7,8,9,4,9,2,1],V.fromList [9,8,5,6,7,8,9,8,9,2],V.fromList [8,7,6,7,8,9,6,7,8,9],V.fromList [9,8,9,9,9,6,5,6,7,8]]}

