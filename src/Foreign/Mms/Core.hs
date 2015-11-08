module Foreign.Mms.Core
    ( Mode(..)
    , ToMms(..)
    , FromMms(..)
    , Put
    , Get
    , putStorable
    , getStorable
    , Storage(..)
    , writeMms
    , getPointer
    , writeOffset
    , loadOffset
    , saveOffset
    ) where

import Control.Monad.State.Strict
import Foreign.Mms.Class(ToMms(..), FromMms(..), GToMms(..), Storage(..))
import Foreign.Mms.Put(
    Put(..), putStorable, saveOffset, loadOffset, writeOffset)
import Foreign.Mms.Get(Get, getStorable, getPointer)
import Foreign.Mms.Builder(toLazyByteString)
import Data.Sequence(null)

import qualified Data.ByteString.Lazy as L

data Mode = Allocated | Mapped

writeMms :: ToMms a => a -> L.ByteString
writeMms x =
    case execState (runPut $ writeData x >> writeFields x) mempty of
        (offsets, builder) | Data.Sequence.null offsets ->
            toLazyByteString builder
        otherwise -> error "Some offsets were not consumed"

