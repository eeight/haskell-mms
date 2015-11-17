module Foreign.Mms.Internal.Builder
    ( Builder
    , storable
    , aligner
    , toLazyByteString
    ) where

import Data.Monoid((<>))
import Foreign.ForeignPtr(ForeignPtr, withForeignPtr)
import Foreign.Ptr(plusPtr)
import Foreign.Storable(Storable(..))
import GHC.ForeignPtr(mallocPlainForeignPtrBytes)
import GHC.Int(Int64)
import GHC.Word(Word8)
import System.IO.Unsafe(unsafePerformIO, unsafeInterleaveIO)

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L

data Buffer = Buffer
    (ForeignPtr Word8) -- Start of the buffer
    Int -- used
    Int -- size

newtype Builder = Builder
    { runBuilder :: (Buffer -> IO L.ByteString) -> Buffer -> IO L.ByteString }

empty :: Builder
empty = Builder ($)

append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (f . g)

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

lazyPrepend :: Buffer -> IO L.ByteString -> IO L.ByteString
lazyPrepend (Buffer p u _) str =
        -- unsafeInterleaveIO here is crucial for lazyness
        L.Chunk (B.PS p 0 u) <$> unsafeInterleaveIO str

ensureBuffer :: Int -> Builder
ensureBuffer n = Builder $ \k b@(Buffer p u s) ->
    if s - u >= n then k b else do
        b' <- newBuffer (max n defaultSize)
        lazyPrepend b (k b')

flush :: Builder
flush = Builder $ \k b@(Buffer p u s) -> lazyPrepend b (k $ Buffer p s s)

writeToBuffer :: Int -> (Buffer -> IO Buffer) -> Builder
writeToBuffer n f = ensureBuffer n <> builder where
    builder = Builder $ \k b -> k =<< f b

toLazyByteString :: Builder -> L.ByteString
toLazyByteString builder = unsafePerformIO $ do
    b <- newBuffer defaultSize
    runBuilder (builder <> flush) (const $ return L.empty) b

storable :: (Storable a) => a -> Builder
storable x = let
    size = sizeOf x
    in writeToBuffer size $ \(Buffer p u s) -> do
        withForeignPtr p $ \p -> poke (p `plusPtr` u) x
        return $ Buffer p (u + size) s

aligner :: Int -> Builder
aligner size = writeToBuffer size $ \(Buffer p u s) -> do
    withForeignPtr p $ \p ->
        B.memset (p `plusPtr` u) 0 (fromIntegral size)
    return $ Buffer p (u + size) s
