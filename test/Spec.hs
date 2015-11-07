import Foreign.Mms

import Test.Hspec

import qualified Data.ByteString.Lazy as L

main :: IO ()
main = hspec $ do
    describe "writeFields" $ do
        it "Double in mms is 8 bytes" $ do
            let d = 1.8 :: Double
            L.length (runPut $ writeFields d) `shouldBe` 8

        it "Point in mms is 16 types" $ do
            let p = Point 1 3
            L.length (runPut $ writeFields p) `shouldBe` 16
