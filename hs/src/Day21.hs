{-# LANGUAGE RecordWildCards #-}
module Day21 where

import qualified Data.MemoTrie as MT
import qualified Debug.Trace as DT

data GameState1 = GameState1 {
  dice    :: Int, -- current times the dice has been rolled so far
  p1Pos   :: Int,
  p1Score :: Int,
  p2Pos   :: Int,
  p2Score :: Int } deriving (Eq, Show)

-- not interested in making an instance for MemoTrie
-- p1Pos, p1Score, p2Pos, p2Score
type GameState2 = (Int, Int)

play :: GameState1 -> Int
play GameState1 { .. } =
    let roll1    = 3 * ((dice + 2) `mod` 10)
        p1Pos'   = (p1Pos + roll1) `mod` 10
        p1Score' = p1Score + score p1Pos'
        roll2 = 3 * ((dice + 5) `mod` 10)
        p2Pos'   = (p2Pos + roll2) `mod` 10
        p2Score' = p2Score + score p2Pos'
    in if p1Score' >= 1000
       then (dice+3) * p2Score
       else if p2Score' >= 1000
       then (dice+6) * p1Score'
       else play $ DT.traceShowId GameState1 {
         dice = dice+6,
         p1Pos = p1Pos',
         p1Score = p1Score',
         p2Pos = p2Pos',
         p2Score = p2Score'}

score :: Int -> Int
score x = if x == 0 then 10 else x

part1 :: Int ->  Int -> Int
part1 p1 p2 = play $ GameState1 0 p1 0 p2 0

encode :: Int -> Int -> Int
encode sc pos = sc * 10 + pos

decode :: Int -> (Int, Int)
decode x = (x `div` 10, x `mod` 10)

-- enumerate all dice role outcomes (sum of three rolls) combined
-- with the number of times they do occur.
duracDiceRolls :: Int -> [Int]
duracDiceRolls 0 = [0] -- with no dice to roll, one outcome w/ sum zero
duracDiceRolls n =
    let prev  = duracDiceRolls (n-1)
        pairs = [k + prs | k <- [1..3], prs <- prev]
    in pairs

-- each step simulates player 1. To make the recursive results useful, we
-- flip the positions on recursive call, and we also flip the results
-- when going back up.
part2 :: Int -> Int -> (Int, Int)
part2 = MT.memo2 (\ player1 player2 -> traceShowWith (decode player1, decode player2) $
          let (s1, p1) = decode player1
              (s2, p2) = decode player2
          in if s2 >= 21 -- only player two can ever win because it was his turn
             then (0, 1)
             else
               let playOn dice = part2 (
                       encode s2 p2) (encode (s1 + score (p1 + dice)) ((p1 + dice) `mod` 10))
                   subres = playOn <$> duracDiceRolls 3
                   (p2Wins, p1Wins) = foldl1 (\(a, b) (c, d) -> (a+c, b+d)) subres
               in (p1Wins, p2Wins))

main :: IO ()
main = do
    let (p1, p2) = part2 4 8
    print $ p1 `max` p2

traceShowWith :: (Show a, Show b) => a -> b -> b
traceShowWith s res = DT.traceShow (show s ++ " -> " ++ show res) res
