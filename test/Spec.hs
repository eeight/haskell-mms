import Mms
import Builder

import Test.Hspec

main :: IO ()
main = hspec $ do
    mmsTest
    builderTest
