module Mms where

import Foreign.Mms
import Foreign.Mms.List
import Foreign.Mms.GVector

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Foreign.Ptr(Ptr, plusPtr)
import GHC.Int(Int64)
import GHC.Generics(Generic)
import Data.Foldable(Foldable(..))

import qualified Data.ByteString.Lazy as L

data Point = Point Double Double deriving (Eq, Show, Generic)

instance ToMms Point
instance FromMms Point

instance Arbitrary Point where
    arbitrary = liftM2 Point arbitrary arbitrary

data SomeData (m :: Mode) = SomeData
    { points :: List m Point
    , numberDouble :: Double
    , numberInt :: Int
    } deriving (Show, Generic)

instance ToMms (SomeData 'Allocated)
instance FromMms (SomeData 'Mapped)

mmsTest:: Spec
mmsTest = do
    describe "writeFields" $ do
        it "Double in mms is 8 bytes" $ do
            let d = 1.8 :: Double
            L.length (writeMms d) `shouldBe` 8

        it "List of two doubles takes 32 bytes" $ do
            let xs = AllocatedList [1.8, 2.4 :: Double]
            L.length (writeMms xs) `shouldBe` 32

        it "Point in mms is 16 bytes" $ do
            let p = Point 1 3
            L.length (writeMms p) `shouldBe` 16

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
            let xs = AllocatedList [1.8, 2.4 :: Double]
            let xs' = readMms . L.toStrict . writeMms $ xs :: List 'Mapped Double
            toList xs' `shouldBe` toList xs

        it "Same for a matrix" $ do
            let xs = AllocatedList (map AllocatedList [[1, 2], [3, 4 :: Double]])
            let xs' = readMms (L.toStrict . writeMms $ xs) :: List 'Mapped (List 'Mapped Double)
            map toList (toList xs') `shouldBe` map toList (toList xs)

        it "Same for SomeData" $ do
            let ed = SomeData (AllocatedList [Point 1 2, Point 3 4]) 1.823 33 :: SomeData 'Allocated
            let ed' = readMms . L.toStrict . writeMms $ ed :: SomeData 'Mapped
            toList (points ed') `shouldBe` toList (points ed)
            numberDouble ed' `shouldBe` numberDouble ed
            numberInt ed' `shouldBe` numberInt ed
