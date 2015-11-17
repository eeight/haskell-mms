module Foreign.Mms.Get (Get(..), getStorable, getPointer, skip) where

import Control.Monad.State.Strict
import Foreign.Ptr(Ptr, plusPtr, castPtr)
import Foreign.Storable(Storable(..))
import GHC.Int(Int64)
import GHC.Word(Word8)

newtype Get a = Get { runGet :: StateT (Ptr Word8) IO a }
    deriving (Functor, Applicative, Monad, MonadState (Ptr Word8), MonadIO)

getStorable :: (Storable a) => Get a
getStorable = do
    ptr <- gets castPtr
    x <- liftIO $ peek ptr
    modify' (`plusPtr` sizeOf x)
    return x

getPointer :: Get (Ptr a)
getPointer = do
    p <- get
    offset <- getStorable :: Get Int64
    return $ p `plusPtr` fromIntegral offset

skip :: Int -> Get ()
skip x = modify' (`plusPtr` x)
