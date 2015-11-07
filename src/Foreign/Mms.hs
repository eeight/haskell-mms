module Foreign.Mms
    ( Mms(..)
    , Put
    , Get
    , putStorable
    , getStorable
    , readMms
    , writeMms
    ) where

import Data.Monoid((<>))
import Foreign.Mms.Builder(Builder, storable, toLazyByteString)
import Foreign.Ptr(Ptr, plusPtr, castPtr)
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.Storable(Storable(..))
import GHC.Word(Word8)
import System.IO.Unsafe(unsafePerformIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L

data PutM a = PutM a Builder
type Put = PutM ()

putStorable :: (Storable a) => a -> Put
putStorable = PutM () . storable

instance Functor PutM where
    fmap f (PutM x b) = PutM (f x) b

instance Applicative PutM where
    pure x = PutM x mempty
    (PutM f b1) <*> (PutM x b2) = PutM (f x) (b1 <> b2)

instance Monad PutM where
    (PutM x b1) >>= f = case f x of PutM y b2 -> PutM y (b1 <> b2)

-- Unrolled ContT r (StateT (Ptr Word8) IO) a
newtype Get a = Get
    { runGet :: forall r . (a -> Ptr Word8 -> IO r) -> Ptr Word8 -> IO r }

instance Functor Get where
    fmap f (Get g) = Get $ \k -> g (k . f)

instance Applicative Get where
    pure x = Get ($ x)
    (Get f) <*> (Get g) = Get $ \k -> f (\t -> g (k .t ))

instance Monad Get where
    (Get f) >>= g = Get $ \k -> f (\x -> runGet (g x) k)

getStorable :: (Storable a) => Get a
getStorable = Get $ \k p -> do
    x <- peek (castPtr p)
    k x (p `plusPtr` sizeOf x)

class Mms m where
    type Freeze m :: *
    writeFields :: m -> Put
    readFields :: Get m

instance Mms Double where
    type Freeze Double = Double
    writeFields = putStorable
    readFields = getStorable

writeMms :: Mms a => a -> L.ByteString
writeMms x = case writeFields x of (PutM () b) -> toLazyByteString b

-- Does not do any bounds checking
readMms :: Mms a => B.ByteString -> a
readMms (B.PS p o _) = unsafePerformIO $
    runGet readFields (\x _ -> return x) (unsafeForeignPtrToPtr p)
