{-# LANGUAGE RecordWildCards #-}

module Day15 where

import Prelude hiding (Left, Right)
import           Data.Function (on)
import           Data.Char (digitToInt)
import           Data.List (sort, groupBy)
import qualified Data.Set as S
import qualified Data.Vector as V
import           Data.Vector ((!))
import qualified Data.Map.Strict as M
import qualified Debug.Trace as DT

-- this is a shortest path problem, on a grid with only the four main direction
-- as connected neighbors
-- - I think "memoization" might not work because we have technically infinite
--   long paths (left-right-left-right-repeat). Of course that's not viable, but
--   `memo` doesn't know that and I don't see an elegant way to teach it to.
-- - we're implementing a simple dijkstra, but the immutability will make our
--   live harder. Recursion will help, but how can we integrate the traversed
--   state into our view?

-- Map Pos Int    -> the "best" distance for each already discovered point
-- Set (Int, Pos) -> priority queue containing the set of unexplored neighbors
--                   - lowest value is the next one to explore, as per dijkstra

class Grid g where
  valueAt :: g -> Point -> Int
  neighbors :: g -> Point -> [Point]
  validPoint :: g -> Point -> Bool

instance Grid SimpleGrid where
  valueAt SimpleGrid { .. } Point { .. } = (grid ! row) ! col
  neighbors g p = filter (validPoint g) $ move p <$> [Left, Right, Up, Down]
  validPoint SimpleGrid { .. } Point { .. } =
    row < rows && row >= 0 && col < cols && col >= 0

newtype ExtendedGrid = ExtendedGrid SimpleGrid

data SimpleGrid = SimpleGrid {
    rows  :: Int,
    cols :: Int,
    grid :: V.Vector (V.Vector Int)
} deriving (Eq, Show)

data Point = Point { row :: Int, col :: Int } deriving (Show, Eq, Ord)

data Direction = Up | Down | Left | Right

move p@Point { .. } Up = p { row = row - 1 }
move p@Point { .. } Down = p { row = row + 1 }
move p@Point { .. } Left = p { col = col - 1 }
move p@Point { .. } Right = p { col = col + 1 }

data Pos = Pos {
    x :: Int,
    y :: Int
} deriving (Eq, Show, Ord)

type Distance = Int

data ReachableNode = ReachableNodei {
    d :: Distance,
    p :: Pos
} deriving (Eq, Show, Ord)


-- search :: Set ReachableNode -> M.Map Pos Distance ->
parseGrid :: [String] -> SimpleGrid
parseGrid ls = SimpleGrid {
    cols = V.length $ V.head grid,
    rows = V.length grid,
    grid = grid
  }
    where grid = V.fromList $ V.fromList <$> ((digitToInt <$>) <$> ls)

go :: SimpleGrid -> S.Set Point -> M.Map Int (S.Set Point) -> Int
go g visited unseen =
  let ((level, points), remaining) = M.deleteFindMin unseen
      reachable = filter (`S.notMember` visited) $
        neighbors g =<< filter (`S.notMember` visited) (S.toList points)
      visited'  = foldl (flip S.insert) visited points
      newUnseen = M.fromList $ map (\xs -> (fst $ head xs, S.fromList $ snd <$> xs)) $
              groupBy ((==) `on` fst) $ sort $ map (\p -> (level + valueAt g p, p)) reachable
      unseen'   = M.unionWith S.union newUnseen remaining
  in if Point { row = rows g - 1, col = cols g - 1 } `elem` points
        then level
        else go g visited' unseen'

part1 :: SimpleGrid -> Int
part1 g = go g S.empty $ M.singleton 0 $ S.singleton Point { row = 0, col = 0}

-- expandGrid :: Grid -> Grid
-- expandGrid Grid { .. } = _

-- part2 :: Grid -> Int
-- part2 = part1 . expandGrid

main :: IO ()
main = do
  ls <- lines <$> getContents
  print . part1 $ parseGrid ls

sampleData :: [String]
sampleData = [
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581"
    ]
