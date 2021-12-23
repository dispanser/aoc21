{-# LANGUAGE RecordWildCards #-}

module Day20 where

import qualified Data.Vector as V
import qualified System.Environment as SE

type PixelMap = V.Vector Bool

data Image = Image {
   pixels  :: V.Vector (V.Vector Bool),
   cols    :: Int,
   rows    :: Int,
   overlay :: Int
}

type Pixel = (Int, Int)

parseImage :: Int -> [String] -> Image
parseImage overlay ls =
    let cols = length (head ls) + 2*overlay
        rows = length ls + 2*overlay
        createRow :: String -> V.Vector Bool
        createRow cs =
            let frame = replicate overlay '.'
            in V.fromList $ (=='#') <$> frame ++ cs ++ frame
        extraLines = replicate overlay $ V.replicate cols False
        pixels = V.fromList $ extraLines ++ (createRow <$> ls) ++ extraLines
    in Image { .. }

printImage :: Image -> IO ()
printImage Image { .. } =
    let createLine line = V.map (\x -> if x then '#' else '.') line
    in  mapM_ print $ createLine <$> pixels

parsePixelMap :: String -> PixelMap
parsePixelMap line = V.fromList $ (=='#') <$> line

getPixel :: Image -> Pixel -> Bool
getPixel Image { .. } (r, c) =
    if r >= 0 && r < rows && c >= 0 && c < cols
       then (pixels V.! r) V.! c
       else (pixels V.! 0) V.! 0

enhanceImage :: PixelMap -> Image -> Image
enhanceImage pm image@Image{..} =
    let mapPixel :: Pixel -> Bool
        mapPixel px = pm V.! getContext image px
        row r = V.generate cols (\c -> mapPixel (r, c))
    in Image
        { rows   = rows,
          cols   = cols,
          pixels = V.generate rows row,
          overlay = overlay
        }

getContext' :: Pixel -> [Pixel]
getContext' (r, c) = reverse $ rows >>= (\row -> repeat row `zip` cols)
 where rows = [r-1 .. r+1]
       cols = [c-1 .. c+1]

getContext :: Image -> Pixel -> Int
getContext image pixel =
    let context = getContext' pixel
        toBinary []     = 0
        toBinary (x:xs) = fromEnum x + 2*toBinary xs
    in toBinary $ getPixel image <$> context

count :: Image -> Int
count Image { .. } = V.sum $ V.length . V.filter id <$> pixels

part1 :: PixelMap -> Image -> Int
part1 pm = count . enhanceImage pm . enhanceImage pm

solve :: Int -> FilePath -> IO ()
solve reps fp = do
    ls <- lines <$> readFile fp
    let pixelMap = parsePixelMap $ head ls
    let overlay = reps + 2
    let image  = parseImage overlay $ drop 2 ls
    let images = iterate (enhanceImage pixelMap)  image
    printImage image
    print $ replicate (rows image) '-'
    printImage $ images !! 1
    print $ replicate (rows image) '-'
    printImage $ images !! 2
    print $ count (images !! reps)

main :: IO ()
main = do
    [fp] <- SE.getArgs
    -- solve 2 fp
    solve 50 fp

