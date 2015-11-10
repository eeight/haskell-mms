module Builder where

import Foreign.Mms.Internal.Builder

import Test.Hspec
import Test.QuickCheck

import Control.Exception(evaluate)
import Data.Monoid((<>))
import GHC.Int(Int64)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B


builderTest:: Spec
builderTest = do
    describe "Builder" $ do
        let n = 1 :: Int64
        let longBuilder = mconcat $  replicate 10000 (storable n)

        it "can handle multi-meg inputs" $ do
            let str = toLazyByteString (mconcat $ replicate 100 longBuilder)
            let str' = L.toStrict str
            L.length str `shouldBe` 8000000
            B.length str' `shouldBe` 8000000

        it "is really lazy" $ do
            -- Verify that the last storable won't be evaluated when only some
            -- prefix of the result is evaluated.
            let builder = longBuilder <> storable (undefined :: Int64)
            let str = toLazyByteString builder
            evaluate $ L.head str
            evaluate $ L.take 10000 str
            evaluate (L.toStrict str) `shouldThrow` errorCall "Prelude.undefined"
