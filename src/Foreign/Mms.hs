module Foreign.Mms
    ( Mms(..)
    , OffsetPopulatingPut
    , OffsetConsumingPut
    , Get
    , putStorable
    , getStorable
    , readMms
    , writeMms
    ) where

import Foreign.Mms.Builder(Builder, storable, writtenSoFar, toLazyByteString)

import Control.Arrow(first, second)
import Control.Monad.State.Strict
import Data.Monoid((<>))
import Data.Sequence(Seq, (|>), viewl, ViewL(..))
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.Ptr(Ptr, plusPtr, castPtr)
import Foreign.Storable(Storable(..))
import GHC.Int(Int64)
import GHC.Word(Word8)
import System.IO.Unsafe(unsafePerformIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L

type Offsets = Seq Int64

newtype Put a = Put { runPut :: State (Offsets, Builder) a }
    deriving (Functor, Applicative, Monad, MonadState (Offsets, Builder))

newtype OffsetPopulatingPut a = Populate
    { runPopulate :: Put a
    } deriving (Functor, Applicative, Monad)

newtype OffsetConsumingPut a = Consume
    { runConsume :: Put a
    } deriving (Functor, Applicative, Monad)

putStorable :: (Storable a) => a -> OffsetConsumingPut ()
putStorable = Consume . modify' . second . (flip mappend) . storable

saveOffset :: OffsetPopulatingPut ()
saveOffset = Populate $ (modify' . first . flip (|>)) =<< (gets (writtenSoFar . snd))

loadOffset :: OffsetConsumingPut Int64
loadOffset = Consume $ gets (viewl . fst) >>= \case
    EmptyL -> error "Trying to consume more offsets than there is."
    offset :< rest -> modify' (first $ const rest) >> return offset

-- Unrolled ContT r (StateT (Ptr Word8) IO) a
newtype Get a = Get
    { runGet :: forall r . (a -> Ptr Word8 -> IO r) -> Ptr Word8 -> IO r }

instance Functor Get where
    fmap f (Get g) = Get $ \k -> g (k . f)

instance Applicative Get where
    pure x = Get ($ x)
    (Get f) <*> (Get g) = Get $ \k -> f (\t -> g (k . t))

instance Monad Get where
    (Get f) >>= g = Get $ \k -> f (\x -> runGet (g x) k)

getStorable :: (Storable a) => Get a
getStorable = Get $ \k p -> do
    x <- peek (castPtr p)
    k x (p `plusPtr` sizeOf x)

class Mms m where
    type Freeze m :: *
    writeFields :: m -> OffsetConsumingPut ()
    writeData :: m -> OffsetConsumingPut ()
    readFields :: Get m

instance Mms Double where
    type Freeze Double = Double
    writeFields = putStorable
    writeData _ = return ()
    readFields = getStorable

writeMms :: Mms a => a -> L.ByteString
writeMms x = let
    (_, builder) = execState (runPut $ runConsume $ writeFields x) mempty
    in toLazyByteString builder

-- Does not do any bounds checking
readMms :: Mms a => B.ByteString -> a
readMms (B.PS p o _) = unsafePerformIO $
    runGet readFields (\x _ -> return x) (unsafeForeignPtrToPtr p)
