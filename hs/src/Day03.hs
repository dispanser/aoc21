module Day03 where

import Data.Bits (testBit)
import Control.Monad (sequence)

countAll :: [String] -> [Int]
countAll xs
    | null (head xs) = []
    | otherwise      = bits : countAll rest
            where
              heads = head <$> xs
              bits  = count (== '1') heads
              rest  = tail <$> xs

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

value :: [Bool] -> Int
value [] = 0
value (x:xs) = (if x then 1 else 0) + 2 * value xs

powerConsumption :: [String] -> Int
powerConsumption ts = gamma * epsilon
    where
        threshold = length ts `div` 2
        counts  = (>threshold) <$> countAll ts
        gamma   = value $ reverse counts
        epsilon = value $ reverse $ not <$> counts

main :: IO ()
main = do
  inputs <- getContents
  print $ countAll $ lines inputs
  print $ powerConsumption $ lines inputs
