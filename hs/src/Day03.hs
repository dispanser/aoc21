module Day03 where

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

-- part 1: 2972336
powerConsumption :: [String] -> Int
powerConsumption ts = gamma * epsilon
    where
        threshold = length ts `div` 2
        counts  = reverse $ (>threshold) <$> countAll ts
        gamma   = value $ counts
        epsilon = value $ not <$> counts

checkBit :: Bool -> [Char] -> Char
checkBit _ [c] = c
checkBit m cs  =
    let ones   = count (== '1') cs
        zeroes = length cs - ones
    in if ones >= zeroes && m || ones < zeroes && (not m)
        then '1'
        else '0'

deriveRating :: Bool -> [String] -> String
deriveRating m ts
    | null (head ts) = []
    | otherwise      =
        let firstColumn      = head <$> ts
            bit              = checkBit m firstColumn
            remainingNumbers = filter (\x -> head x == bit) ts
            tails            = tail <$> remainingNumbers
        in bit : deriveRating m tails

valueString :: String -> Int
valueString ts = value $ reverse ((== '1') <$> ts)

-- part 2: 3368358
liveSupportRating :: [String] -> Int
liveSupportRating ts = oxygen * scrubber
    where
        oxygen   = valueString $ deriveRating True ts
        scrubber = valueString $ deriveRating False ts

main :: IO ()
main = do
  inputs <- getContents
  print $ powerConsumption $ lines inputs
  print $ liveSupportRating $ lines inputs
