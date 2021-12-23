import Test.Hspec
import Test.Tasty.Hspec
import Test.Tasty

import Text.Parsec (parse)
import Day22

main = tests >>= defaultMain . testGroup "all hspec tests"

tests :: IO [TestTree]
tests = sequence [
  testSpec "parser tests" testParse ,
  testSpec "part 1 tests" testFindingCubeState ]

-- testGroup "Tests" [
  -- [ testParseToggle
  -- , testParseNumber
  -- , testParseRange
  -- , testParseCommand
  -- ]

-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT

--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]

testParse = describe "parse input line" $ do
    it "should correctly parse on/off keyword" $ do
        parse parseToggle "" "on" `shouldBe` Right On
        parse parseToggle "" "off" `shouldBe` Right Off
    it "should parse positive and negative numbers" $ do
        parse parseNumber "" "13"  `shouldBe` Right 13
        parse parseNumber "" "+13" `shouldBe` Right 13
        parse parseNumber "" "-13" `shouldBe` Right (-13)
    it "should parse ranges of numbers min..max" $ do
        parse parseRange "" "13..14"   `shouldBe` Right (Range 13 14)
        parse parseRange "" "-0..8"    `shouldBe` Right (Range 0 8)
        parse parseRange "" "-13..-3"  `shouldBe` Right (Range (-13) (-3))
    it "should parse entire commands " $ do
        parse parseCommand "" "on x=10..12,y=10..12,z=10..12" `shouldBe`
            Right (Command On (Range 10 12) (Range 10 12) (Range 10 12))
        parse parseCommand "" "off x=9..11,y=9..11,z=9..11" `shouldBe`
            Right (Command Off (Range 9 11) (Range 9 11) (Range 9 11))

testFindingCubeState = describe "handling part 1" $ do
    it "should match cube into boxes defined by commands" $ do
        matches (Command On (Range 10 12) (Range 10 12) (Range 10 12)) (10, 10, 10)
            `shouldBe` True
        matches (Command On (Range 11 12) (Range 11 12) (Range 11 12)) (10, 10, 10)
            `shouldBe` False
    it "should identify cube state for lack of matching commads" $ do
        isCubeOn [ Command On (Range 11 12) (Range 11 12) (Range 11 12)
                 , Command On (Range 3 3) (Range 17 28) (Range 19 23)
                 ] (7, 20, 20) `shouldBe` False
    it "should identify cube state with both 'on' and 'off' matching commads" $ do
        isCubeOn [ Command On (Range 0 12) (Range 11 22) (Range 11 22)
                 , Command Off (Range 3 3) (Range 17 28) (Range 19 23)
                 ] (3, 20, 20) `shouldBe` True
    it "should identify cube state with both 'on' and 'off' matching commads" $ do
        isCubeOn [ Command Off (Range 0 12) (Range 11 22) (Range 11 22)
                 , Command On  (Range 3 3) (Range 17 28) (Range 19 23)
                 ] (3, 20, 20) `shouldBe` False



test01 :: Spec
test01 =
    describe "on some toy example" $ do
        -- perfect line: y = 2x-1
        it "should correctly run some test" $ do
            13 + 4 `shouldBe`17
