module Layout where

import Foreign.Mms.Internal.Layout

import Test.Hspec

import Control.Exception(evaluate)
import Data.Monoid((<>))

layoutShouldBe l (ps, s) = do
    paddings l `shouldBe` ps
    structSize l `shouldBe` s

layoutTest:: Spec
layoutTest = do
    describe "Layout is computed correctly for" $ do
        it "a pair of ints" $ do
            struct [builtin 4, builtin 4] `layoutShouldBe` ([0, 0], 8)

        it "a pair of int and char" $ do
            struct [builtin 4, builtin 1] `layoutShouldBe` ([0, 3], 8)

        it "int char double char" $ do
            let s = struct (map builtin [4, 1, 8, 1])
            s `layoutShouldBe` ([0, 3, 0, 7], 24)

        it "{char int} double char char short" $ do
            let s = struct (map builtin [1, 4])
            let ss = struct [s, builtin 8, builtin 1, builtin 1, builtin 2]
            ss `layoutShouldBe` ([0, 0, 0, 0, 4], 24)
