module Foreign.Mms
    ( ToMms(..)
    , FromMms(..)
    , OffsetPopulatingPut
    , OffsetConsumingPut
    , Get
    , putStorable
    , getStorable
    , Storage(..)
    , writeMms
    , getPointer
    , writeOffset
    , loadOffset
    , saveOffset
--    , populateOffsets
--    , consumeOffsets
    ) where

import Foreign.Mms.Builder(Builder, storable, writtenSoFar, toLazyByteString)

import Control.Arrow(first, second)
import Control.Monad.State.Strict
import Data.Monoid((<>))
import Data.Sequence(Seq, (|>), viewl, ViewL(..), null)
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

--withOffsets :: Offsets -> Put a -> Put (a, Offsets)
--withOffsets offsets action = do
--    (savedOffsets, builder) <- get
--    let (x, (offsets', builder')) = runState action (offsets, builder)
--    put (savedOffsets, builder')
--    return (x, offsets')

type OffsetPopulatingPut = Put
type OffsetConsumingPut = Put

putStorable :: (Storable a) => a -> OffsetConsumingPut ()
putStorable = modify' . second . (flip mappend) . storable

saveOffset :: OffsetPopulatingPut ()
saveOffset = (modify' . first . flip (|>)) =<< (gets (writtenSoFar . snd))

loadOffset :: OffsetConsumingPut Int64
loadOffset = gets (viewl . fst) >>= \case
    EmptyL -> error "Trying to consume more offsets than there is."
    offset :< rest -> modify' (first $ const rest) >> return offset

writeOffset :: Int64 -> OffsetConsumingPut ()
writeOffset offset = do
     offsetNow <- gets (writtenSoFar . snd)
     putStorable (offset - offsetNow)

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

getPointer :: Get (Ptr a)
getPointer = do
    p <- Get $ \k p -> k p p
    offset <- getStorable :: Get Int64
    return $ p `plusPtr` (fromIntegral offset)

class ToMms a where
    type Freeze a :: *
    writeData :: a -> OffsetPopulatingPut ()
    writeFields :: a -> OffsetConsumingPut ()

class FromMms a where
    mmsSize :: a -> Int
    readFields :: Get a

instance ToMms Double where
    type Freeze Double = Double
    writeData _ = return ()
    writeFields = putStorable

instance FromMms Double where
    mmsSize x = sizeOf x
    readFields = getStorable

writeMms :: ToMms a => a -> L.ByteString
writeMms x = let
    (offsets, builder) = execState (runPut $ writeData x >> writeFields x) mempty
    in if Data.Sequence.null offsets
        then toLazyByteString builder
        else error "Some offsets were not consumed"

class Storage a where
    readMms :: FromMms b => a -> b

instance Storage (Ptr a) where
    readMms = unsafePerformIO . runGet readFields (\x _ -> return x) . castPtr

instance Storage B.ByteString where
    readMms (B.PS p o s) = let
        headSize = mmsSize result
        beginPtr = unsafeForeignPtrToPtr p
        result = readMms (beginPtr `plusPtr` (s - headSize))
        in result
