{-# LANGUAGE RecordWildCards #-}

module Day08 where

import Control.Monad (forM_)
import Data.Function (on)
import Data.List (sortBy, sort)
import qualified Data.Map as M
import           Data.Map ((!))

data DisplayReadings = DisplayReadings {
    patterns :: [String],
    readings :: [String]
} deriving (Eq, Show)

parseInput :: String -> DisplayReadings
parseInput input = DisplayReadings p r
    where
     ws = sort <$> words input
     p  = take 10 ws
     r  = drop 11 ws

supersetOf :: Eq a => [a] -> [a] -> Bool
supersetOf xs = all (`elem` xs)

commonSegments :: Eq a => [a] -> [a] -> Int
commonSegments xs = length . filter  (`elem` xs)

deriveDigitMapping :: [String] -> M.Map String Int
deriveDigitMapping patterns =
   let ps    = sortBy (compare `on` length) patterns
       one   = head ps  -- two segments lit, straight forward
       seven = ps !! 1  -- three segments lit
       four  = ps !! 2  -- four segments lit
       eight = last ps
       s235  = take 3 $ drop 3 ps
       s069  = take 3 $ drop 6 ps
       [three] = filter (`supersetOf` seven) s235
       [five]  = filter (\p -> commonSegments p four == 3 && not (supersetOf p seven)) s235
       [two]   = filter (\p -> commonSegments p four == 2) s235
       [zero]  = filter (\p -> p `supersetOf` seven && not (p `supersetOf` four)) s069
       [six]   = filter (\p -> not (p `supersetOf` seven)) s069
       [nine]  = filter (\p -> p `supersetOf` seven && p `supersetOf` four) s069
    in M.fromList [
        (one, 1),
        (seven, 7),
        (four, 4),
        (eight, 8),
        (three, 3),
        (five, 5),
        (two, 2),
        (zero, 0),
        (six, 6),
        (nine, 9)
    ]

resolveDisplayValue :: M.Map String Int -> [String] -> Int
resolveDisplayValue digitLookup [d3, d2, d1, d0] =
    digitLookup ! d3 * 1000 + digitLookup ! d2 * 100 +
    digitLookup ! d1 * 10 + digitLookup ! d0

solveSingle :: DisplayReadings -> Int
solveSingle DisplayReadings {..} =
   let digitMapping = deriveDigitMapping patterns
   in  resolveDisplayValue digitMapping readings

part2 :: [DisplayReadings] -> Int
part2 drs = sum $ solveSingle <$> drs

-- -> 9: 6 segments, superset of segmets of 4 [cefabd] and 7 [abd]
-- -> 6: 6 segments, superset of 4 but not 7
-- -> 0: 6 segments, superset of 7 but not 4

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
    let input = parseInput <$> ls
    print . part1 $ input
    print . part2 $ input

row1 :: DisplayReadings
row1 = parseInput "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"

row0 :: DisplayReadings
row0 = parseInput "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
