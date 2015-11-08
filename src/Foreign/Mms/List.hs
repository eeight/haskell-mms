module Foreign.Mms.List(List(..)) where

import Foreign.Mms.GVector(GVector(..))
import Foreign.Mms.MappedVector(MappedVector)
import Foreign.Mms.Core
    ( Mode(..)
    , ToMms(..)
    , FromMms(..)
    , saveOffset
    , loadOffset
    , writeOffset
    , putStorable
    )
import GHC.Int(Int64)

data List (m :: Mode) a where
    AllocatedList :: { unAllocatedList :: [a] } -> List 'Allocated a
    MappedList :: {unMappedList :: MappedVector a } -> List 'Mapped a

deriving instance Foldable (List m)
deriving instance Show a => Show (List m a)

instance FromMms a => FromMms (List 'Mapped a) where
    mmsSize ~(MappedList v) = mmsSize v
    readFields = MappedList <$> readFields

instance GVector (List 'Allocated) a where
    glength = length . unAllocatedList
    at = (!!) . unAllocatedList

instance GVector (List 'Mapped) a where
    glength = glength . unMappedList
    at = at . unMappedList

instance ToMms a => ToMms (List 'Allocated a) where
    writeData (AllocatedList xs) =
        mapM_ writeData xs >> saveOffset >> mapM_ writeFields xs
    writeFields (AllocatedList xs) = do
        writeOffset =<< loadOffset
        putStorable $ (fromIntegral (length xs) :: Int64)
