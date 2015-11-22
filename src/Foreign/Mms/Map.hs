{-# LANGUAGE UndecidableInstances #-}
module Foreign.Mms.Map(Map(..), null, size, member, lookup, toList) where

import Prelude hiding (lookup, null)

import Data.Maybe(isJust)
import Foreign.Mms.Class(Mms(..))
import Foreign.Mms.Core(Mode(..))
import Foreign.Mms.Put(saveOffset)

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Foreign.Mms.MappedVector as V
import qualified Foreign.Mms.Vector as V

data Map (m :: Mode) k v where
    AllocatedMap :: { unAllocatedMap :: M.Map k v } -> Map 'Allocated k v
    MappedMap :: { unMappedMap :: V.MappedVector (k, v) } -> Map 'Mapped k v

deriving instance (Show k, Show v) => Show (Map m k v)

-- This instance does not satisfy coverage condition for the fundeps
-- of Mms class, hence the UndecidableInstances. It however satisfies
-- weak coverage condition, so it's mostly fineFromMms.
instance (Mms ka km, Mms va vm) =>
        Mms (Map 'Allocated ka va) (Map 'Mapped km vm) where
    writeData (AllocatedMap m) = let
        pairs = M.toList m
        in mapM_ writeData pairs >> saveOffset >> mapM_ writeFields pairs
    writeFields = V.mappedVectorWriteFields .
        fromIntegral . M.size . unAllocatedMap

    mmsSize _ = V.mappedVectorSize
    mmsAlignment _ = V.mappedVectorAlignment
    readFields = MappedMap <$> V.mappedVectorReadFields

null :: Map 'Mapped k v -> Bool
null = V.null . unMappedMap

size :: Map 'Mapped k v -> Int
size = V.length . unMappedMap

member :: Ord k => Map 'Mapped k v -> k -> Bool
member m = isJust . lookup m

lookup :: Ord k => Map 'Mapped k v -> k -> Maybe v
lookup (MappedMap items) x = go 0 (V.length items) where
    go l r
        | l == r = Nothing
        | otherwise = let
            m = (l + r) `div` 2
            (k, v) = items V.! m
            in case compare x k of
                LT -> go l m
                GT -> go m r
                EQ -> Just v

toList :: Map 'Mapped k v -> [(k, v)]
toList = F.toList . unMappedMap
