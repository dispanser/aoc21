module Day14 where

import           Control.Monad (mapM_)
import           Data.Either (rights)
import           Data.List (sort, group)
import qualified Data.Map.Strict as M
import           Data.Map.Strict ((!?))
import           Data.Maybe (fromMaybe)

import Text.Parsec (char, many1, digit, string, spaces, parse, letter)
import Text.Parsec.String (Parser)

type RuleSet = M.Map (Char, Char) String

applyRule :: RuleSet -> Char -> Char -> String
applyRule rs c1 c2 = fromMaybe [c1, c2] $ rs !? (c1, c2)

parseRule :: Parser ((Char, Char), String)
parseRule = do
  c1 <- letter
  c2 <- letter
  _  <- spaces *> string "->" *> spaces
  r <- letter
  return ((c1, c2), [r, c2])

parseRules :: [String] -> RuleSet
parseRules rs = M.fromList .rights $ parse parseRule "" <$> rs

step :: RuleSet -> String -> String
step rs inp = head inp : concat (zipWith (applyRule rs) inp $ tail inp)

part1 :: RuleSet -> String -> Int
part1 rs inp =
  let iterations = iterate (step rs) inp
      tenth      = sort $ iterations !! 10
      chars      = length <$> group tenth
  in maximum chars - minimum chars


main :: IO ()
main = do
  ls <- lines <$> getContents
  let input = head ls
  let rules = parseRules (drop 2 ls)
  let iterations = take 5 $ iterate (step rules) input
  mapM_ print iterations
  print $ part1 rules input


