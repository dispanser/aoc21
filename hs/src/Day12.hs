{-# LANGUAGE RecordWildCards #-}

module Day12 where

import           Data.Char (isUpper)
import           Data.Function (on)
import           Data.List (groupBy, sort, sortBy)
import           Data.List.Extra (split)
import qualified Data.Map.Strict as M
import           Data.Map.Strict ((!))
import qualified Debug.Trace as D

data Vertex = Vertex {
    name :: String,
    big  :: Bool
  } deriving (Eq, Ord)

instance Show Vertex where
  show v = show $ name v

-- path is interpreted in reverse order, i.e. we're pushing additional
-- path element at the front.
type Path = [Vertex]

newtype Graph = Graph {
    neighbors :: M.Map String [Vertex]
  } deriving (Eq, Show)

-- checks that a vertex can be added to the (logical) end of a path
validExtensionPart1 :: Path -> Vertex -> Bool
validExtensionPart1 vs v = big v || v `notElem` vs

-- checks that a vertex can be added to the (logical) end of a path
validExtensionPart2 :: Path -> Vertex -> Bool
validExtensionPart2 vs v =
  let smallVs       = sortBy (compare `on` name) $ filter (not . big) (v:vs)
      distinctNames = groupBy ((==) `on` name) smallVs
  in (name v /= "start") && length distinctNames + 1 >= length smallVs

cursePath :: (Path -> Vertex -> Bool) -> Graph -> Path -> Int
cursePath _ _ [] = error "need at least one (start) element for a path"
cursePath p g@Graph { .. } path@(tip:rest)
    | name tip == "end" = 1  -- terminal condition: reached the end
    | otherwise =
    -- we know there's a neighbor because we came from somewhere
    let successors = filter (p path) $ neighbors ! name tip
    in  sum $ cursePath p g . (:path) <$> successors

parseEdge :: String -> [(String, Vertex)]
parseEdge l =
  let [n1, n2] = split (=='-') l
      v1 = Vertex { name = n1, big = all isUpper n1 }
      v2 = Vertex { name = n2, big = all isUpper n2 }
  in [(n1, v2), (n2, v1)]

parseGraph :: [String] -> Graph
parseGraph ls =
  let edges     = sort $ ls >>= parseEdge
      grouped   = groupBy ((==) `on` fst) edges
      neighbors = M.fromList $ (\es -> (fst $ head es, snd <$> es)) <$> grouped
  in  Graph { .. }

part1 :: Graph -> Int
part1 g = cursePath validExtensionPart1 g [Vertex { name = "start", big = False }]

part2 :: Graph -> Int
part2 g = cursePath validExtensionPart2 g [Vertex { name = "start", big = False }]

main :: IO ()
main = do
  ls <- lines <$> getContents
  print $ part1 $ parseGraph ls
  print $ part2 $ parseGraph ls

ex1 :: [String]
ex1 = [
    "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end" ]



