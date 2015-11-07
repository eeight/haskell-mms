module Foreign.Mms.Builder
    ( Builder
    , storable
    , toLazyByteString
    ) where

import Foreign.Ptr(plusPtr)
import Foreign.ForeignPtr(ForeignPtr, withForeignPtr)
import Foreign.Storable(Storable(..))
import GHC.ForeignPtr(mallocPlainForeignPtrBytes)
import GHC.Word(Word8)
import System.IO.Unsafe(unsafePerformIO)

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L

data Buffer = Buffer
    (ForeignPtr Word8) -- Start of the buffer
    Int -- used
    Int -- size

newtype Builder = Builder
    { runBuilder :: (Buffer -> IO L.ByteString) -> Buffer -> IO L.ByteString
    }

empty :: Builder
empty = Builder ($)

append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder $ f . g

instance Monoid Builder where
    mempty = empty
    mappend = append

-- Copied from Data.ByteString.Lazy
defaultSize :: Int
defaultSize = 32 * k - overhead where
    k = 1024
    overhead = 2 * sizeOf (undefined :: Int)

newBuffer :: Int -> IO Buffer
newBuffer s = do
    p <- mallocPlainForeignPtrBytes s
    return $ Buffer p 0 s

ensureBuffer :: Int -> Builder
ensureBuffer n = Builder $ \k b@(Buffer p u s) ->
    if s - u >= n then k b else do
        b <- newBuffer (max n defaultSize)
        L.Chunk (B.PS p 0 u) <$> k b

flush :: Builder
flush = Builder $ \k (Buffer p u s) ->
    L.Chunk (B.PS p 0 u) <$> k (Buffer p s s)

withBuffer :: (Buffer -> IO Buffer) -> Builder
withBuffer f = Builder $ \k b -> k =<< f b

toLazyByteString :: Builder -> L.ByteString
toLazyByteString builder = unsafePerformIO $ do
    b <- newBuffer defaultSize
    runBuilder (builder `append` flush) (const $ return L.empty) b

storable :: (Storable a) => a -> Builder
storable x = ensureBuffer size `append` write where
    size = sizeOf x
    write = withBuffer $ \(Buffer p u s) -> do
        withForeignPtr p $ \p -> poke (p `plusPtr` u) x
        return $ Buffer p (u + size) s

