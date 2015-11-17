module Mms where

import Foreign.Mms
import Foreign.Mms.List
import Foreign.Mms.GVector

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Foreign.Ptr(Ptr, plusPtr)
import GHC.Int(Int64, Int8)
import GHC.Generics(Generic)
import Data.Foldable(Foldable(..))

import qualified Data.ByteString.Lazy as L

data Point = Point Double Double deriving (Eq, Show, Generic)

instance Mms Point Point

instance Arbitrary Point where
    arbitrary = liftM2 Point arbitrary arbitrary

data SomeData (m :: Mode) = SomeData
    { points :: List m Point
    , numberInt :: Int8
    , numberDouble :: Double
    } deriving (Show, Generic)

instance Mms (SomeData 'Allocated) (SomeData 'Mapped)

mmsTest:: Spec
mmsTest = do
    describe "writeFields has predictable size" $ do
        it "Double is 8 bytes" $ do
            let d = 1.8 :: Double
            L.length (writeMms d) `shouldBe` 8

        it "List of two doubles is 32 bytes" $ do
            let xs = AllocatedList [1.8, 2.4 :: Double]
            L.length (writeMms xs) `shouldBe` 32

        it "Point is 16 bytes" $ do
            let p = Point 1 3
            L.length (writeMms p) `shouldBe` 16

        it "SomeData is 32 bytes" $ do
            let sd =  SomeData (AllocatedList []) 33 1.823
            L.length (writeMms sd) `shouldBe` 32

    describe "readFields . toStrict . writeFields = id" $ do
        let
            prop :: (Mms a a, Eq a) => a -> Bool
            prop x = (readMms . L.toStrict . writeMms) x == x
        it "Double" $ quickCheck (prop :: Double -> Bool)
        it "Point" $ quickCheck (prop :: Point -> Bool)

    describe "Variable-length data has predictable size" $ do
        it "Two-component vector" $ do
            let xs = AllocatedList [1.8, 2.4 :: Double]
            let xs' = readMms . L.toStrict . writeMms $ xs :: List 'Mapped Double
            toList xs' `shouldBe` toList xs

        it "Matrix" $ do
            let xs = AllocatedList (map AllocatedList [[1, 2], [3, 4 :: Double]])
            let xs' = readMms (L.toStrict . writeMms $ xs) :: List 'Mapped (List 'Mapped Double)
            map toList (toList xs') `shouldBe` map toList (toList xs)

        it "SomeData" $ do
            let sd = SomeData (AllocatedList [Point 1 2, Point 3 4]) 33 1.823
            let sd' = readMms . L.toStrict . writeMms $ sd :: SomeData 'Mapped
            toList (points sd') `shouldBe` toList (points sd)
            numberDouble sd' `shouldBe` numberDouble sd
            numberInt sd' `shouldBe` numberInt sd
