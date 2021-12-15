

module Day14 where

import           Data.MemoTrie (memo, memo2)
import           Control.Monad (mapM_)
import           Data.Either (rights)
import           Data.List (sort, group)
import qualified Data.Map.Strict as M
import           Data.Map.Strict ((!?), (!))
import           Data.Maybe (fromMaybe)

import Text.Parsec (char, many1, digit, string, spaces, parse, letter)
import Text.Parsec.String (Parser)

type RuleSet = M.Map (Char, Char) Char

type Result = M.Map Char Int

parseRule :: Parser ((Char, Char), Char)
parseRule = do
  c1 <- letter
  c2 <- letter
  _  <- spaces *> string "->" *> spaces
  r <- letter
  return ((c1, c2), r)

parseRules :: [String] -> RuleSet
parseRules rs = M.fromList .rights $ parse parseRule "" <$> rs

-- non-memoizing recursive implementation, but in contrast to
-- part 1, with a signature that's suitable for memoization
part2Rec :: RuleSet -> Int -> (Char, Char) -> Result
part2Rec rs 0        ( _, c2) = M.singleton c2 1
part2Rec rs count cs@(c1, c2) =
  let inbetweenChar = rs ! cs
  in M.unionWith (+) (part2Rec rs (count-1) (c1, inbetweenChar))
                     (part2Rec rs (count-1) (inbetweenChar, c2))

-- memo edition
part2Rec' :: RuleSet -> Int -> (Char, Char) -> Result
part2Rec' rs = part2Rec''
  where part2Rec'' = memo2 (\ count cs@(c1, c2) ->
              case count of
                0 -> M.singleton c2 1
                n ->
                  let inbetweenChar = rs ! cs
                  in M.unionWith (+) (part2Rec'' (count-1) (c1, inbetweenChar))
                     (part2Rec'' (count-1) (inbetweenChar, c2))
            )


part2 :: RuleSet -> Int -> String -> Result
part2 rs count inp =
  let pairs          = inp `zip` tail inp
      partialResults = part2Rec' rs count <$> pairs
      charCounts     = foldl (M.unionWith (+)) (M.singleton (head inp) 1) partialResults
  in charCounts

main :: IO ()
main = do
  ls <- lines <$> getContents
  let input = head ls
  let rules = parseRules (drop 2 ls)
  let resp1 = part2 rules 10 input
  let countsp1 = snd <$> M.toList resp1
  let res = part2 rules 40 input
  let counts = snd <$> M.toList res
  print (maximum countsp1 - minimum countsp1)
  print (maximum counts - minimum counts)

