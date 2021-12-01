module Day01 where

readInts :: IO [Int]
readInts = do
  c <- getContents
  return $ map read $ lines c

increasing :: [Int] -> [Int] -> Int
increasing x1 x2 = length $ filter (uncurry (<)) $ zip x1 x2

part1 :: IO ()
part1 = do
  xs <- readInts
  print $ increasing xs $ tail xs

part2 :: IO ()
part2 = do
  xs <- readInts
  print $ increasing xs $ drop 3 xs

main :: IO ()
main = part2
