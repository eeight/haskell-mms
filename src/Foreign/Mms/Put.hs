module Foreign.Mms.Put
    (Put(..), putStorable, saveOffset, loadOffset, writeOffset) where

import Control.Arrow(first, second)
import Control.Monad.State.Strict
import Data.Sequence(Seq, (|>), viewl, ViewL(..), null)
import Foreign.Mms.Builder(Builder, storable, writtenSoFar, toLazyByteString)
import Foreign.Storable(Storable(..))
import GHC.Int(Int64)

type Offsets = Seq Int64

newtype Put a = Put { runPut :: State (Offsets, Builder) a }
    deriving (Functor, Applicative, Monad, MonadState (Offsets, Builder))

putStorable :: (Storable a) => a -> Put ()
putStorable = modify' . second . flip mappend . storable

saveOffset :: Put ()
saveOffset = modify' . first . flip (|>) =<< gets (writtenSoFar . snd)

loadOffset :: Put Int64
loadOffset = gets (viewl . fst) >>= \case
    EmptyL -> error "Trying to consume more offsets than there is."
    offset :< rest -> modify' (first $ const rest) >> return offset

writeOffset :: Int64 -> Put ()
writeOffset offset = do
     offsetNow <- gets (writtenSoFar . snd)
     putStorable (offset - offsetNow)

