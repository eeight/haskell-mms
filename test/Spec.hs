import Mms
import Builder
import Layout

import Test.Hspec

main :: IO ()
main = hspec $ do
    mmsTest
    builderTest
    layoutTest
