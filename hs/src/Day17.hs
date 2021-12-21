{-# LANGUAGE RecordWildCards #-}
module Day17 where

{-
    - termination condidtion:
        - probe is past the rightmost x coordinate
        - probe is past the lowest y coordinate **and going down**
    - how do we restrict the search space?
        - it feels like x and y are completely independent:
        - x is dragged down by 1
        - y follows gravity
        - the choice of x can impact the choice of y:
          a different step could be the one "in range"
        - other than that, the y velocity could be super high,
          it just has to fit on its way down at the right step
        - when going down, the y coordinate (on the parable) is
          touching 0 again, after that continues to accelerate.
          so the upper limit is a step size of y' (min) because then
          it touches the lowest y of the boundary at the next step.
     - given target area: x=207..263, y=-115..-63
       - -115 is the accel, before that we have -114
       - solution is 114*115/2 == 6555
    - min x: x that barely reaches the lower x boundary (207),
      square function, ...
    - max x: the x that directly shoots to the rightmost x (207)
    - min y: directly shoot to the lower y bound
    - max y: see part1 solution
    - 200x200 -> 40k options, just simulate the trajectory, and
      terminate with "false" when going past the thing in either dir
-}

part1 :: Int -> Int
part1 lowerY = abs lowerY*(abs lowerY - 1) `div` 2

velocityX :: Int -> [Int]
velocityX n
  | n == 0 = repeat 0
  | n > 0  = n : velocityX  (pred n)
  | n < 0  = n : velocityX  (succ n)

velocityY :: Int -> [Int]
velocityY = iterate pred

inRange :: Bounds-> Int -> Bool
inRange Bounds { .. } n = upper >= n && lower <= n

inBox :: Box -> (Int, Int) -> Bool
inBox Box { .. } (x, y) = inRange xRange x && inRange yRange y

-- given the y-bounds of the target box, compute all y trajectories that
-- properly touch the target box on their way down
yTrajectories :: Bounds -> [[Int]]
yTrajectories bounds@Bounds { .. } =
  let trajectories = scanl1 (+) . velocityY <$>  [lower .. abs lower]
      candidates   = takeWhile (>= lower) <$>  trajectories
  in filter (any (inRange bounds)) candidates

minXToReachBox :: Int -> Int
minXToReachBox lowerX = floor $ sqrt $ 2 * fromIntegral lowerX

data Bounds = Bounds {
    lower :: Int,
    upper :: Int } deriving (Eq, Show, Ord)

data Box = Box {
    xRange :: Bounds,
    yRange :: Bounds } deriving (Eq, Show, Ord)

-- trajectory :: Bounds -> [Int] -> [Int] -> Bool
-- trajectory = _

trajectory :: [Int] -> Int -> [(Int, Int)]
trajectory ys xInit =
  let xs = (scanl1 (+) $ velocityX xInit)
  in  xs `zip` ys

validTrajectory :: Box -> [(Int, Int)] -> Bool
validTrajectory box = any (inBox box)

part2 :: Box -> Int
part2 box@Box { .. } =
  let allXs = [minXToReachBox (lower xRange) .. (upper xRange)]
      allTrajectories = yTrajectories yRange >>= (\ys -> trajectory ys <$> allXs)
  in length $ filter (validTrajectory box) allTrajectories

testBox = Box (Bounds 20 30) (Bounds (-10) (-5))
prodBox = Box (Bounds 207 263) (Bounds (-115) (-63))

