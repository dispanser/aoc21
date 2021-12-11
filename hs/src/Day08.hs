module Day08 where

import Control.Monad (forM_)

data DisplayReadings = DisplayReadings {
    patterns :: [String],
    readings :: [String]
} deriving (Eq, Show)

parseInput :: String -> DisplayReadings
parseInput input = DisplayReadings p r
    where
     ws = words input
     p  = take 10 ws
     r  = drop 11 ws


part1 :: [DisplayReadings] -> Int
part1 drs = sum $ map count1478 drs

is1478 :: String -> Bool
is1478 p = length p `elem` [2, 3, 4, 7]

count1478 :: DisplayReadings -> Int
count1478 dr = length $ filter is1478 $ readings dr

main :: IO ()
main = do
    ls <- lines <$> getContents
    forM_ ls $ print . count1478 . parseInput
    print . part1 $ parseInput <$> ls

row1 :: DisplayReadings
row1 = parseInput "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
