{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Day16 where

import           Data.Bits (testBit)
import           Data.Char (digitToInt)
import           Data.Bifunctor (first, second)
{--
the implementation itself probably isn't complicated, just a bunch of rules to
follow. But the basic "parsing interface" is a different subject matter. Should
I create an abstraction that is able to produce bits on request?

- BitStream / BitReader, sort of... hm, isn't that just a list of bits, lazily
- mapped from the underlying hex numbers? yeah, that's about it I think

--}
-- for all I know, literals can be very large; use Integer

data PacketContents = Literal Integer | SubPackets [Packet] deriving (Show, Eq)

data Packet = Packet {
    version  :: Int,
    typeId   :: Int,
    contents :: PacketContents
} deriving (Show, Eq)

parseDigit :: Char -> [Int]
parseDigit c = fromEnum . testBit (digitToInt c) <$> [3, 2 .. 0]

parseInput :: String -> [Int]
parseInput v = v >>= parseDigit

parsePacketsLength :: [Int] -> (PacketContents, [Int])
parsePacketsLength bits =
  let (len, rest)      = first readInt $ splitAt 15 bits
      (payload, rest') = splitAt len rest
      parsePackets :: [Int] -> [Packet]
      parsePackets [] = []
      parsePackets xs =
        let (p, rs) = parsePacket xs
        in  p : parsePackets rs
  in  (SubPackets (parsePackets payload), rest')

parsePacketsCount :: [Int] -> (PacketContents, [Int])
parsePacketsCount bits =
  let (count, rest) = first readInt $ splitAt 11 bits
      parsePackets :: Int -> [Int] -> ([Packet], [Int])
      parsePackets 0 rs = ([], rs)
      parsePackets n rs =
        let (p, rs') = parsePacket rs
        in first (p:) $ parsePackets (n-1) rs'
      (packets, rest') = parsePackets count rest
  in (SubPackets packets, rest')

parsePacket :: [Int] -> (Packet, [Int])
parsePacket bits =
  let (v, rest)  = first readInt $ splitAt 3 bits
      (t, rest') = first readInt $ splitAt 3 rest
      (content, rest'')
        | t == 4          = parseLiteral rest'
        | head rest' == 0 = parsePacketsLength (tail rest')
        | otherwise       = parsePacketsCount  (tail rest')
  in (Packet v t content, rest'')

parseLiteral :: [Int] -> (PacketContents, [Int])
parseLiteral xs = first (Literal . fromIntegral . readInt) $ go xs
  where go xs' = let (bits, rest) = splitAt 5 xs'
                 in if head bits == 0
                   then (tail bits, rest)
                   else
                     let (recBits, rest') = go rest
                     in  (tail bits ++ recBits, rest')

-- converts a sequence of digits into an int
readInt :: [Int] -> Int
readInt bits =
  let powersOfTwo = (2^) <$> [0..]
  in  sum $ zipWith (*) powersOfTwo $ reverse bits

part1 :: Packet -> Int
part1 Packet { .. } =
  case contents of
    Literal _     -> version
    SubPackets ps -> version + sum (part1 <$> ps)

part2 :: Packet -> Integer
part2 = \case
  Packet _ 0 (SubPackets ps) -> sum $ part2 <$> ps
  Packet _ 1 (SubPackets ps) -> product $ part2 <$> ps
  Packet _ 2 (SubPackets ps) -> minimum $ part2 <$> ps
  Packet _ 3 (SubPackets ps) -> maximum $ part2 <$> ps
  Packet _ 4 (Literal n)     -> n
  Packet _ 5 (SubPackets [p1, p2]) -> if part2 p1 > part2 p2 then 1 else 0
  Packet _ 6 (SubPackets [p1, p2]) -> if part2 p1 < part2 p2 then 1 else 0
  Packet _ 7 (SubPackets [p1, p2]) -> if part2 p1 == part2 p2 then 1 else 0
  p                      -> error $ "unsupported package: " ++ show p

main :: IO ()
main = do
  ls <- lines <$> getContents
  let packets = parsePacket . parseInput <$> ls
  mapM_ print $ first part1 <$> packets
  mapM_ print $ first part2 <$> packets
  -- mapM_ print $ part1 . parsePacket <$> examples

examples :: [String]
examples = [
    "D2FE28",
    "38006F45291200",
    "EE00D40C823060",
    "8A004A801A8002F478",
    "620080001611562C8802118E34",
    "C0015000016115A2E0802F182340",
    "A0016C880162017C3686B18A3D4780" ]
