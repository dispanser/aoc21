module Day10 where

-- plan: recurse! pop off a matching closing delimiter; append
-- an opening char when observed; mismatched closing gets a score
-- end of char stream gets a 0
part1LineScore :: String -> String -> Int
part1LineScore es (c:cs)
    | c `elem` ['(', '[', '{', '<'] = part1LineScore (c:es) cs
part1LineScore ('[':es) (']':cs) = part1LineScore es cs -- matching []
part1LineScore ('(':es) (')':cs) = part1LineScore es cs -- matching ()
part1LineScore ('{':es) ('}':cs) = part1LineScore es cs -- matching {}
part1LineScore ('<':es) ('>':cs) = part1LineScore es cs -- matching <>
part1LineScore _ (')':_) = 3
part1LineScore _ (']':_) = 57
part1LineScore _ ('}':_) = 1197
part1LineScore _ ('>':_) = 25137
part1LineScore _ [] = 0   -- empty input, icomplete line


part1 :: [String] -> Int
part1 ls = sum $ part1LineScore "" <$> ls

main :: IO ()
main = do
  ls <- lines <$> getContents
  print $ part1 ls
