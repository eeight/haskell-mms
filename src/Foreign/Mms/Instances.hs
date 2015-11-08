module Foreign.Mms.Instances() where

import Control.Monad.State.Strict
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.Mms.Class
import Foreign.Mms.Get(runGet, getStorable)
import Foreign.Mms.Put(putStorable)
import Foreign.Ptr(Ptr, plusPtr, castPtr)
import Foreign.Storable(Storable(..))
import GHC.Generics(K1(..), M1(..), (:*:)(..), (:+:))
import GHC.Int(Int8, Int16, Int32, Int64)
import System.IO.Unsafe(unsafePerformIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

instance ToMms Int where
    writeData _ = return ()
    writeFields = putStorable

instance FromMms Int where
    mmsSize = sizeOf
    readFields = getStorable

instance ToMms Int8 where
    writeData _ = return ()
    writeFields = putStorable

instance FromMms Int8 where
    mmsSize = sizeOf
    readFields = getStorable

instance ToMms Int16 where
    writeData _ = return ()
    writeFields = putStorable

instance FromMms Int16 where
    mmsSize = sizeOf
    readFields = getStorable

instance ToMms Int32 where
    writeData _ = return ()
    writeFields = putStorable

instance FromMms Int32 where
    mmsSize = sizeOf
    readFields = getStorable

instance ToMms Int64 where
    writeData _ = return ()
    writeFields = putStorable

instance FromMms Int64 where
    mmsSize = sizeOf
    readFields = getStorable

instance ToMms Float where
    writeData _ = return ()
    writeFields = putStorable

instance FromMms Float where
    mmsSize = sizeOf
    readFields = getStorable

instance ToMms Double where
    writeData _ = return ()
    writeFields = putStorable

instance FromMms Double where
    mmsSize = sizeOf
    readFields = getStorable

instance ToMms a => GToMms (K1 i a) where
    gwriteData (K1 x) = writeData x
    gwriteFields (K1 x) = writeFields x

instance GToMms a => GToMms (M1 i c a) where
    gwriteData (M1 x) = gwriteData x
    gwriteFields (M1 x) = gwriteFields x

instance (GToMms a, GToMms b) => GToMms (a :*: b) where
    gwriteData (x :*: y) = gwriteData x >> gwriteData y
    gwriteFields (x :*: y) = gwriteFields x >> gwriteFields y

instance FromMms a => GFromMms (K1 i a) where
    gmmsSize ~(K1 x) = mmsSize x
    greadFields = K1 <$> readFields

instance GFromMms a => GFromMms (M1 i c a) where
     gmmsSize ~(M1 x) = gmmsSize x
     greadFields = M1 <$> greadFields

instance (GFromMms a, GFromMms b) => GFromMms (a :*: b) where
    gmmsSize ~(x :*: y) = gmmsSize x + gmmsSize y
    greadFields = liftM2 (:*:) greadFields greadFields

instance Storage (Ptr a) where
    readMms = unsafePerformIO . evalStateT (runGet readFields) . castPtr

instance Storage B.ByteString where
    readMms (B.PS p o s) = let
        headSize = mmsSize result
        beginPtr = unsafeForeignPtrToPtr p
        result = readMms (beginPtr `plusPtr` (s - headSize))
        in result
