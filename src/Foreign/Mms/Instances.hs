{-# LANGUAGE UndecidableInstances #-}
module Foreign.Mms.Instances() where

import Control.Monad.State.Strict
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.Mms.Class
import Foreign.Mms.Get(runGet, getStorable, skip)
import Foreign.Mms.Internal.Layout(Layout(..), builtin, mkLayout)
import Foreign.Mms.Put(putStorable, pad)
import Foreign.Ptr(Ptr, plusPtr, castPtr)
import Foreign.Storable(Storable(..))
import GHC.Generics(K1(..), M1(..), (:*:)(..), (:+:))
import GHC.Int(Int8, Int16, Int32, Int64)
import System.IO.Unsafe(unsafePerformIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

instance Mms Int Int where
    writeData _ = return ()
    writeFields = putStorable

    mmsSize = sizeOf
    mmsAlignment = alignment
    readFields = getStorable

instance Mms Int8 Int8 where
    writeData _ = return ()
    writeFields = putStorable

    mmsSize = sizeOf
    mmsAlignment = alignment
    readFields = getStorable

instance Mms Int16 Int16 where
    writeData _ = return ()
    writeFields = putStorable

    mmsSize = sizeOf
    mmsAlignment = alignment
    readFields = getStorable

instance Mms Int32 Int32 where
    writeData _ = return ()
    writeFields = putStorable

    mmsSize = sizeOf
    mmsAlignment = alignment
    readFields = getStorable

instance Mms Int64 Int64 where
    writeData _ = return ()
    writeFields = putStorable

    mmsSize = sizeOf
    mmsAlignment = alignment
    readFields = getStorable

instance Mms Float Float where
    writeData _ = return ()
    writeFields = putStorable

    mmsSize = sizeOf
    mmsAlignment = alignment
    readFields = getStorable

instance Mms Double Double where
    writeData _ = return ()
    writeFields = putStorable

    mmsSize = sizeOf
    mmsAlignment = alignment
    readFields = getStorable

-- All of the following instances break coverage condition for functional
-- dependencies of GMms. However, they satisfy weak coverage condition
-- which still ensures typechecker termination. We use UndecidableInstances
-- to allow this instances to exist.
instance Mms a m => GMms (K1 i a) (K1 i m)  where
    gwriteData (K1 x) = writeData x
    gwriteFields (K1 x) [p] = writeFields x >> pad p
    gfields (K1 x) = [mkLayout (mmsAlignment x) (mmsSize x)]
    greadFields [p] = K1 <$> (readFields <* skip p)

instance GMms a m => GMms (M1 i c a) (M1 i c m) where
    gwriteData (M1 x) = gwriteData x
    gwriteFields (M1 x) ps = gwriteFields x ps
    gfields (M1 x) = gfields x
    greadFields ps = M1 <$> greadFields ps

instance (Mms a m, GMms fa fm) => GMms (K1 i a :*: fa) (K1 i m :*: fm) where
    gwriteData ((K1 x) :*: y) = writeData x >> gwriteData y
    gwriteFields ((K1 x) :*: y) (p:ps) = writeFields x >> pad p >> gwriteFields y ps
    gfields ~((K1 x) :*: y) = mkLayout (mmsAlignment x) (mmsSize x) : gfields y
    greadFields (p:ps)= do
        x <- readFields
        skip p
        y <- greadFields ps
        return $ (K1 x) :*: y

instance (GMms fa fm, GMms fa' fm') => GMms (M1 i c fa :*: fa') (M1 i c fm :*: fm') where
    gwriteData ((M1 x) :*: y) = gwriteData x >> gwriteData y
    gwriteFields ((M1 x) :*: y) (p:ps) = gwriteFields x [p] >> gwriteFields y ps
    gfields ~((M1 x) :*: y) = gfields x ++ gfields y
    greadFields (p:ps) = do
        x <- greadFields [p]
        y <- greadFields ps
        return $ (M1 x) :*: y

instance Storage (Ptr a) where
    readMms = unsafePerformIO . evalStateT (runGet readFields) . castPtr

instance Storage B.ByteString where
    readMms (B.PS p o s) = let
        headSize = mmsSize result
        beginPtr = unsafeForeignPtrToPtr p
        result = readMms (beginPtr `plusPtr` (s - headSize))
        in result
