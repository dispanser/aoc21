{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Day04 where

import qualified Data.IntMap as M
import           Data.List (minimumBy, maximumBy)
import           Data.List.Extra (split, chunksOf)
import           Data.Function (on)
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed ((!))
import           Control.Monad (forM_, join)

-- lookup: which position was each input seen at?
type PositionMap = M.IntMap Int

data Solution = Solution { atMove :: Int
                         , score :: Int } deriving (Show, Eq)
    -- sortBy (compare `on` fst)

type BingoBoard = V.Vector Int

data Position = Position { row :: Int, col :: Int }

createPositionLookup :: V.Vector Int -> PositionMap
createPositionLookup vs = M.fromList (V.toList vs `zip` [0..])

parseNumbers :: String -> V.Vector Int
parseNumbers line = V.fromList $ read <$> split (==',') line

maxInRow :: Int -> BingoBoard -> Int
maxInRow row = V.maximum . V.slice (5*row) 5

maxInCol :: Int -> BingoBoard -> Int
maxInCol col board = maximum $ (board !) <$> [col, col+5 ..  24]

createBoard :: [String] -> BingoBoard
createBoard ls =
    let ws = words =<< ls
        ns = read <$> ws
    in V.fromList ns

solvedBoard :: PositionMap -> BingoBoard -> BingoBoard
solvedBoard positionMap = V.map (positionMap M.!)

printBoard :: BingoBoard -> IO ()
printBoard board = forM_ (chunksOf 5 $ V.toList board) print

solvedAtMove :: BingoBoard -> Int
solvedAtMove board =
    let offsets = [0 .. 4] :: [Int]
        bestRow = (`maxInRow` board) <$> offsets
        bestCol = (`maxInCol` board) <$> offsets
    in minimum $ bestRow ++ bestCol

solveBoard :: V.Vector Int -> BingoBoard -> Solution
solveBoard numbers board =
    let positionMap = createPositionLookup numbers
        movePositionBoard = solvedBoard positionMap board
        move = solvedAtMove movePositionBoard
        numThatSolves = numbers ! move
        uncheckedNumbers = V.sum $ (\i -> positionMap M.! i > move) `V.filter` board
        s = Solution { atMove = move, score = uncheckedNumbers * numThatSolves }
    in s

main :: IO ()
main = do
     ls <- lines <$> getContents
     let numbers = parseNumbers $ head ls
     let positionMap = createPositionLookup numbers
     let boards      = createBoard <$> chunksOf 5 (filter (not . null) $ tail ls)
     let boardSolutions = solveBoard numbers <$> boards
     print $ minimumBy (compare `on` atMove) boardSolutions
     print $ maximumBy (compare `on` atMove) boardSolutions
