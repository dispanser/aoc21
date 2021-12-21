{-# LANGUAGE RecordWildCards #-}
module Day21 where

import qualified Debug.Trace as DT

data GameState1 = GameState1 {
  dice    :: Int, -- current times the dice has been rolled so far
  p1Pos   :: Int,
  p1Score :: Int,
  p2Pos   :: Int,
  p2Score :: Int } deriving (Eq, Show)

play :: GameState1 -> Int
play GameState1 { .. } =
    let roll1    = 3 * ((dice + 2) `mod` 10)
        p1Pos'   = (p1Pos + roll1) `mod` 10
        p1Score' = p1Score + if p1Pos' == 0 then 10 else p1Pos'
        roll2 = 3 * ((dice + 5) `mod` 10)
        p2Pos'   = (p2Pos + roll2) `mod` 10
        p2Score' = p2Score + if p2Pos' == 0 then 10 else p2Pos'
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

part1 :: Int ->  Int -> Int
part1 p1 p2 = play $ GameState1 0 p1 0 p2 0
