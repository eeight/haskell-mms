import Foreign.Mms

import Control.Monad(liftM2)
import Test.Hspec
import Test.QuickCheck
import Foreign.Storable(Storable)

import qualified Data.ByteString.Lazy as L

data Point = Point Double Double deriving (Eq, Show)

-- Explicitly make Point and instance of Mms instead of making it
-- storable and using default instance.
instance Mms Point where
    type Freeze Point = Point
    writeFields (Point x y) = mapM_ writeFields [x, y]
    readFields = liftM2 Point readFields readFields

instance Arbitrary Point where
    arbitrary = liftM2 Point arbitrary arbitrary

main :: IO ()
main = hspec $ do
    describe "writeFields" $ do
        it "Double in mms is 8 bytes" $ do
            let d = 1.8 :: Double
            L.length (writeMms d) `shouldBe` 8

        it "Point in mms is 16 types" $ do
            let p = Point 1 3
            L.length (writeMms p) `shouldBe` 16

    describe "readFields . toStrict . writeFields = id" $ do
        let
            prop :: (Mms a, Eq a) => a -> Bool
            prop x = (readMms . L.toStrict . writeMms) x == x
        it "Holds for double" $ do
            quickCheck (prop :: Double -> Bool)
        it "Holds for Point" $ do
            quickCheck (prop :: Point -> Bool)
