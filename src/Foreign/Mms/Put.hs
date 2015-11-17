module Foreign.Mms.Put
    ( Put
    , evalPut
    , putStorable
    , saveOffset
    , loadOffset
    , writeOffset
    , pad
    ) where

import Control.Arrow(first, second)
import Control.Monad.State.Strict
import Data.Sequence(Seq, (|>), viewl, ViewL(..), null)
import Foreign.Mms.Internal.Builder(
    Builder, storable, aligner, toLazyByteString)
import Foreign.Storable(Storable(..))
import GHC.Int(Int64)

import qualified Data.ByteString.Lazy as L

type Offsets = Seq Int64

data PutState = PutState
    { offsets :: Offsets
    , builder :: Builder
    , writtenSoFar :: Int64
    }

modifyOffsets :: (Offsets -> Offsets) -> PutState -> PutState
modifyOffsets f s = s{offsets = f $ offsets s}

modifyBuilder :: (Builder -> Builder) -> PutState -> PutState
modifyBuilder f s = s{builder = f $ builder s}

modifyWrittenSoFar :: (Int64 -> Int64) -> PutState -> PutState
modifyWrittenSoFar f s = s{writtenSoFar = f $ writtenSoFar s}

newtype Put a = Put { runPut :: State PutState a }
    deriving (Functor, Applicative, Monad, MonadState PutState)

putStorable :: (Storable a) => a -> Put ()
putStorable x = modify' $
    modifyBuilder (`mappend` storable x) .
    modifyWrittenSoFar (+ fromIntegral (sizeOf x))

saveOffset :: Put ()
saveOffset = do
    written <- gets writtenSoFar
    modify' $ modifyOffsets (|> written)

loadOffset :: Put Int64
loadOffset = gets (viewl . offsets) >>= \case
    EmptyL -> error "Trying to consume more offsets than there is."
    offset :< rest -> modify' (modifyOffsets $ const rest) >> return offset

writeOffset :: Int64 -> Put ()
writeOffset offset = do
     offsetNow <- gets writtenSoFar
     putStorable (offset - offsetNow)

pad :: Int -> Put ()
pad 0 = return ()
pad x = modify' $
    modifyWrittenSoFar (+ fromIntegral x) .
    modifyBuilder (`mappend` aligner x)

evalPut :: Put () -> L.ByteString
evalPut (Put p) = let
    emptyState = PutState{offsets = mempty, builder = mempty, writtenSoFar = 0}
    PutState{offsets, builder} = execState p (PutState mempty mempty 0)
    in if Data.Sequence.null offsets
        then toLazyByteString builder
        else error "Some offsets were not consumed"
