module Day02 where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Either (rights)
import Data.Foldable (foldl')

data Command =
    Forward Int
    | Down Int
    | Up Int
    deriving (Show)

type Pos2 = (Int, Int)
type Pos3 = (Int, Int, Int)

readLines :: IO [String]
readLines = lines <$> getContents

readCommands :: IO [Command]
readCommands = do
  lines <- readLines
  let parsedLines = parse commandParser "" <$> lines
  return $ rights parsedLines

numberParser :: Parsec String st Int
numberParser = do
  num <- many1 digit
  return $ read num

commandParser :: Parser Command
commandParser =
  string "forward " *> (Forward <$> numberParser)
          <|> string "up "  *> (Up <$> numberParser)
          <|> string "down " *> (Down <$> numberParser)

applyCommand1 :: Pos2 -> Command -> Pos2
applyCommand1 (h, p) (Forward i) = (h, p + i)
applyCommand1 (h, p) (Down i) = (h+i, p)
applyCommand1 (h, p) (Up i) = (h-i, p)

applyCommand2 :: Pos3 -> Command -> Pos3
applyCommand2 (h, p, a) (Forward i) = (h + i*a, p+i, a)
applyCommand2 (h, p, a) (Down i) = (h, p, a+i)
applyCommand2 (h, p, a) (Up i) = (h, p, a-i)


part1 :: [Command] -> Pos2
part1 = foldl' applyCommand1 (0, 0)

part2 :: [Command] -> Pos3
part2 = foldl' applyCommand2 (0, 0, 0)

main :: IO ()
main = do
  commands <- readCommands
  let (h, p, _) = part2 commands
  print $ h * p
