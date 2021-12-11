module Day07 where


import Data.List (sort, group)
import Data.List.Extra (split)

part1 :: [Int] -> Int
part1 numbers =
  let l      = length numbers
      sorted = sort numbers
      median = sorted !! (l `div` 2)
  in sum $ abs . (median -) <$> sorted

part2brute :: [Int] -> Int
part2brute numbers =
  let low    = minimum numbers
      high   = maximum numbers
      costSingle delta = (abs delta * succ (abs delta)) `div` 2
      costAll n        = sum $ costSingle . (n-) <$> numbers
  in  minimum $ costAll <$> [low .. high]

main :: IO ()
main = do
  tokens <- split (== ',') <$> getContents
  let numbers = read <$> tokens :: [Int]
  let sumx = sum numbers
  let num  = length numbers
  print $ part1 numbers
  print $ part2brute numbers
