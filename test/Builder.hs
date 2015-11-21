module Builder where

import Foreign.Mms.Internal.Builder

import Test.Hspec

import Control.Monad(forM_)
import Control.Exception(evaluate)
import Data.Monoid((<>))
import GHC.Int(Int64)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B


builderTest:: Spec
builderTest = do
    describe "Builder" $ do
        let n = 1 :: Int64
        let longBuilder i = mconcat $  replicate i (storable n)

        it "puts aligners correctly" $ do
            let str = toLazyByteString $ mconcat $ replicate 1000 (aligner 7)
            L.length str `shouldBe` 7000
            L.unpack str `shouldSatisfy` all (== 0)

        it "puts bytestring correctly" $ let
            bstr = B.pack [1,2..100]
            builder = storable n <> splice bstr <> storable n
            str = toLazyByteString builder
            in L.length str `shouldBe` 116

        it "writes bytes in correct numbers" $ do
            forM_ [1..1000] $ \i -> let
                str = toLazyByteString (longBuilder i)
                in L.length str `shouldBe` (fromIntegral i*8)

        it "can handle multi-meg inputs" $ do
            let str = toLazyByteString $ longBuilder 10000000
            let str' = L.toStrict str
            L.length str `shouldBe` 80000000
            B.length str' `shouldBe` 80000000

        it "is really lazy" $ do
            -- Verify that the last storable won't be evaluated when only some
            -- prefix of the result is evaluated.
            let builder = longBuilder 10000 <> undefined
            let str = toLazyByteString builder
            evaluate $ L.head str
            evaluate $ L.take 10000 str
            evaluate (L.toStrict str) `shouldThrow` errorCall "Prelude.undefined"
