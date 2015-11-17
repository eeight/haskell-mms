module Foreign.Mms.Core(Mode(..), writeMms) where

import Control.Monad.State.Strict
import Foreign.Mms.Class(Mms(..))
import Foreign.Mms.Put(evalPut)

import qualified Data.ByteString.Lazy as L

data Mode = Allocated | Mapped

writeMms :: Mms a m => a -> L.ByteString
writeMms x = evalPut $ writeData x >> writeFields x
