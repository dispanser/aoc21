{-# LANGUAGE RecordWildCards #-}
module Day06 where

import Data.List (sort, group)
import Data.List.Extra (split)
import Foreign (callocArray0)

-- number of fish for a given age
-- we don't simulate individual fish, but all fish
data FishGroup = FishGroup {
    reprodTimer :: Int,
    numFish     :: Int } deriving (Show, Eq, Ord)

advanceFish :: FishGroup -> [FishGroup]
advanceFish FishGroup { .. }
    | reprodTimer > 0 = [FishGroup { reprodTimer = reprodTimer - 1, numFish = numFish }]
    | otherwise       = [
        FishGroup { reprodTimer = 6, numFish = numFish },
        FishGroup { reprodTimer = 8, numFish = numFish } ]

mergeGroups :: [FishGroup] -> [FishGroup]
mergeGroups fishGroups =
  let mergeGroup :: [FishGroup] -> FishGroup
      mergeGroup fgs@(fg:_) = FishGroup {
          reprodTimer = reprodTimer fg,
          numFish     = sum $ numFish<$> fgs
      }
      -- grouped = group $ sort fishGroups
  in  mergeGroup <$> group (sort fishGroups)

countFish :: [FishGroup] -> Int
countFish = sum . (numFish <$>)

part1 :: [FishGroup] -> Int
part1 fishGroups =
  let advance fgs = mergeGroups $ advanceFish =<< fgs
  in countFish (iterate advance fishGroups !! 80)

-- TODO:
main :: IO ()
main = do
  tokens <- split (== ',') <$> getContents
  let fishgroups = (`FishGroup` 1) . read <$> tokens
  print $ part1 fishgroups
