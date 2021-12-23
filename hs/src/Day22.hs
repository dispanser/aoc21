{-# LANGUAGE RecordWildCards #-}

module Day22 where

-- for each cube
-- go from end, search for the first (last) range that contains it
-- this attempts to use lazyness to speed things up a bit

-- for each cube
-- go from end, search for the first (last) range that contains it
-- this attempts to use lazyness to speed things up a bit
import qualified Text.Parsec as P
import           Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import Data.Functor (($>))
import Data.Either (rights)
import Data.Maybe  (listToMaybe)

data Range = Range { lower :: Int, upper :: Int } deriving (Eq, Show, Ord)

data Toggle = On | Off deriving (Eq, Show, Ord)

type Point = (Int, Int, Int)

data Command = Command {
  toggle ::  Toggle,
  xs     :: Range,
  ys     :: Range,
  zs     :: Range } deriving (Eq, Show, Ord)

parseToggle :: Parser Toggle
parseToggle = P.try (P.string "on" $> On) <|> P.string "off" $> Off

parseNumber :: Parser Int
parseNumber = do
    sign   <- P.option '+' (P.oneOf "+-")
    digits <- read <$> P.many1 P.digit
    return (if sign == '-' then -digits else digits)

parseRange :: Parser Range
parseRange = do
    lower <- parseNumber
    _     <- P.string ".."
    upper <- parseNumber
    return $ Range { .. }

parseCommand :: Parser Command
parseCommand = do
    toggle <- parseToggle
    xs     <- P.spaces *> P.string "x=" *> parseRange
    ys     <- P.char ',' *> P.string "y=" *> parseRange
    zs     <- P.char ',' *> P.string "z=" *> parseRange
    return Command { .. }

matches :: Command -> Point -> Bool
matches Command { .. } (x, y, z) =
    lower xs <= x && upper xs >= x &&
        lower ys <= y && upper ys >= y &&
        lower zs <= z && upper zs >= z

isCubeOn :: [Command] -> Point -> Bool
isCubeOn cs point = case filter (`matches` point) cs of
                      []    -> False
                      (x:_) -> toggle x == On

-- parse all the inputs. return in reverse order because later
-- commands "override" older commands
readInput :: FilePath -> IO [Command]
readInput fp = do
    ls <- lines <$> readFile fp
    return . reverse . rights $ P.parse parseCommand "" <$> ls



