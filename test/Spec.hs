import Foreign.Mms

import Test.Hspec
import Test.QuickCheck

import Control.Monad(liftM2)
import Foreign.Ptr(Ptr, plusPtr)
import Foreign.Storable(Storable)
import GHC.Int(Int64)
import Control.Monad

import Debug.Trace

import qualified Data.ByteString.Lazy as L

data Point = Point Double Double deriving (Eq, Show)

-- Explicitly make Point and instance of Mms instead of making it
-- storable and using default instance.
instance ToMms Point where
    type Freeze Point = Point
    writeData _ = return ()
    writeFields (Point x y) = mapM_ writeFields [x, y]

instance FromMms Point where
    mmsSize _ = 16
    readFields = liftM2 Point readFields readFields

newtype AllocatedVector a = AllocatedVector
    { unAllocated :: [a]
    } deriving (Foldable)

data MappedVector a = MappedVector (Ptr a) Int64

toList :: FromMms a => MappedVector a -> [a]
toList (MappedVector p length) = let
    xs = take (fromIntegral length) $ map readMms $
        iterate (`plusPtr` elementSize) p
    elementSize = mmsSize $ head xs
    in xs

instance ToMms a => ToMms (AllocatedVector a) where
    type Freeze (AllocatedVector a) = MappedVector (Freeze a)

    writeData xs = mapM_ writeData xs >> saveOffset >> mapM_ writeFields xs
    writeFields xs = do
        writeOffset =<< loadOffset
        putStorable $ (fromIntegral (length xs) :: Int64)

instance FromMms a => FromMms (MappedVector a) where
    mmsSize _ = 16
    readFields = liftM2 MappedVector getPointer getStorable

instance Arbitrary Point where
    arbitrary = liftM2 Point arbitrary arbitrary

main :: IO ()
main = hspec $ do
    describe "writeFields" $ do
        it "Double in mms is 8 bytes" $ do
            let d = 1.8 :: Double
            L.length (writeMms d) `shouldBe` 8

        it "Point in mms is 16 bytes" $ do
            let p = Point 1 3
            L.length (writeMms p) `shouldBe` 16

        it "Vector of two doubles takes 32 bytes" $ do
            let xs = AllocatedVector [1.8, 2.4] :: AllocatedVector Double
            L.length (writeMms xs) `shouldBe` 32

    describe "readFields . toStrict . writeFields = id" $ do
        let
            prop :: (ToMms a, FromMms a, Eq a) => a -> Bool
            prop x = (readMms . L.toStrict . writeMms) x == x
        it "Holds for double" $ do
            quickCheck (prop :: Double -> Bool)
        it "Holds for Point" $ do
            quickCheck (prop :: Point -> Bool)

    describe "Write variable-length data" $ do
        it "Two-component vector can be retrieved back" $ do
            let xs = AllocatedVector [1.8, 2.4 :: Double]
            let xs' = readMms (L.toStrict . writeMms $ xs) :: MappedVector Double
            toList xs' `shouldBe` unAllocated xs

        it "Same for a matrix " $ do
            let xs = AllocatedVector (map AllocatedVector [[1, 2], [3, 4 :: Double]])
            let xs' = readMms (L.toStrict . writeMms $ xs) :: MappedVector (MappedVector Double)
            map toList (toList xs') `shouldBe` map unAllocated (unAllocated xs)
