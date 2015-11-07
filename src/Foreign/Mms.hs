module Foreign.Mms where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.ForeignPtr(mallocPlainForeignPtrBytes)
import GHC.Word(Word8)
import System.IO.Unsafe(unsafePerformIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L

data Buffer = Buffer
    (ForeignPtr Word8) -- Start of the buffer
    Int -- used
    Int -- size

data Builder = Builder
    { runBuilder :: (Buffer -> IO L.ByteString) -> Buffer -> IO L.ByteString
    }

empty :: Builder
empty = Builder ($)

append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder $ f.g

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

toByteString :: Builder -> L.ByteString
toByteString builder = unsafePerformIO $ do
    b <- newBuffer defaultSize
    runBuilder (builder `append` flush) (const $ return L.empty) b

data PutM a = PutM a Builder
type Put = PutM ()

runPut :: Put -> L.ByteString
runPut (PutM () b) = toByteString b

put :: (Storable a) => a -> Put
put x = PutM () (ensureBuffer size `append` write) where
    size = sizeOf x
    write = withBuffer $ \(Buffer p u s) -> do
        withForeignPtr p $ \p -> poke (p `plusPtr` u) x
        return $ Buffer p (u + size) s

instance Functor PutM where
    fmap f (PutM x b) = PutM (f x) b

instance Applicative PutM where
    pure x = PutM x empty
    (PutM f b1) <*> (PutM x b2) = PutM (f x) (b1 `append` b2)

instance Monad PutM where
    (PutM x b1) >>= f = case f x of PutM y b2 -> PutM y (b1 `append` b2)

class Mms m where
    type Freeze m :: *
    writeFields :: m -> Put

instance Mms Double where
    type Freeze Double = Double
    writeFields = put

data Point = Point Double Double

instance Mms Point where
    type Freeze Point = Point
    writeFields (Point x y) = mapM_ writeFields [x, y]
