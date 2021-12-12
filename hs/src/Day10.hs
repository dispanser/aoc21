{-# LANGUAGE LambdaCase #-}

module Day10 where

import Foreign (callocArray0)
import Data.Maybe (mapMaybe)
import Data.List (sort)

data LineAnalysis =
    CorruptLine Int |
    IncompleteLine String

-- plan: recurse! pop off a matching closing delimiter; append
-- an opening char when observed; mismatched closing gets a score
-- end of char stream gets a 0
lineAnalysis :: String -> String -> LineAnalysis
lineAnalysis es (c:cs)
    | c `elem` ['(', '[', '{', '<'] = lineAnalysis (c:es) cs
lineAnalysis ('[':es) (']':cs) = lineAnalysis es cs -- matching []
lineAnalysis ('(':es) (')':cs) = lineAnalysis es cs -- matching ()
lineAnalysis ('{':es) ('}':cs) = lineAnalysis es cs -- matching {}
lineAnalysis ('<':es) ('>':cs) = lineAnalysis es cs -- matching <>
lineAnalysis _ (')':_) = CorruptLine 3
lineAnalysis _ (']':_) = CorruptLine 57
lineAnalysis _ ('}':_) = CorruptLine 1197
lineAnalysis _ ('>':_) = CorruptLine 25137
lineAnalysis expected  [] = IncompleteLine expected


analyseLines :: [String] -> [LineAnalysis]
analyseLines ls = lineAnalysis "" <$> ls

part1 :: [String] -> Int
part1 ls =
  let lsa        = analyseLines ls
      lineScores = mapMaybe (\case
              CorruptLine x    -> Just x
              IncompleteLine _ -> Nothing)
            lsa
  in sum lineScores

scoreIncomplete :: String -> Int
scoreIncomplete [] = 0
scoreIncomplete ('(':xs) = 1 + 5 * scoreIncomplete xs
scoreIncomplete ('[':xs) = 2 + 5 * scoreIncomplete xs
scoreIncomplete ('{':xs) = 3 + 5 * scoreIncomplete xs
scoreIncomplete ('<':xs) = 4 + 5 * scoreIncomplete xs

part2 :: [String] -> Int
part2 ls =
  let lsa        = analyseLines ls
      lineScores = mapMaybe (\case
              CorruptLine _    -> Nothing
              IncompleteLine e -> Just . scoreIncomplete $ reverse e)
            lsa
      medianIdx  = length lineScores `div` 2
  in sort lineScores !! medianIdx

main :: IO ()
main = do
  ls <- lines <$> getContents
  print $ part1 ls
  print $ part2 ls
