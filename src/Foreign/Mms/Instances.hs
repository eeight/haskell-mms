module Foreign.Mms.Instances() where

import Control.Monad.State.Strict
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.Mms.Class
import Foreign.Mms.Get(runGet, getStorable)
import Foreign.Mms.Put(putStorable)
import Foreign.Ptr(Ptr, plusPtr, castPtr)
import Foreign.Storable(Storable(..))
import GHC.Generics(U1(..), K1(..), M1(..), (:*:)(..), (:+:))
import System.IO.Unsafe(unsafePerformIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

instance ToMms Double where
    writeData _ = return ()
    writeFields = putStorable

instance FromMms Double where
    mmsSize = sizeOf
    readFields = getStorable

instance GToMms U1 where
    gwriteData _ = return ()
    gwriteFields _ = return ()

instance ToMms a => GToMms (K1 i a) where
    gwriteData (K1 x) = writeData x
    gwriteFields (K1 x) = writeFields x

instance GToMms a => GToMms (M1 i c a) where
    gwriteData (M1 x) = gwriteData x
    gwriteFields (M1 x) = gwriteFields x

instance (GToMms a, GToMms b) => GToMms (a :*: b) where
    gwriteData (x :*: y) = gwriteData x >> gwriteData y
    gwriteFields (x :*: y) = gwriteFields x >> gwriteFields y

instance Storage (Ptr a) where
    readMms = unsafePerformIO . evalStateT (runGet readFields) . castPtr

instance Storage B.ByteString where
    readMms (B.PS p o s) = let
        headSize = mmsSize result
        beginPtr = unsafeForeignPtrToPtr p
        result = readMms (beginPtr `plusPtr` (s - headSize))
        in result