{-# LANGUAGE UndecidableInstances #-}
module Foreign.Mms.List(List(..)) where

import Foreign.Mms.GVector(GVector(..))
import Foreign.Mms.MappedVector
import Foreign.Mms.Core(Mode(..))
import Foreign.Mms.Class(Mms(..))
import Foreign.Mms.Put(saveOffset, loadOffset , writeOffset)
import GHC.Int(Int64)

data List (m :: Mode) a where
    AllocatedList :: { unAllocatedList :: [a] } -> List 'Allocated a
    MappedList :: { unMappedList :: MappedVector a } -> List 'Mapped a

deriving instance Foldable (List m)
deriving instance Show a => Show (List m a)

-- This instance does not satisfy coverage condition for the fundeps
-- of Mms class, hence the UndecidableInstances. It however satisfies
-- weak coverage condition, so it's mostly fineFromMms.
instance Mms a m => Mms (List 'Allocated a) (List 'Mapped m) where
    writeData (AllocatedList xs) =
        mapM_ writeData xs >> saveOffset >> mapM_ writeFields xs
    writeFields (AllocatedList xs) = do
        writeOffset =<< loadOffset
        writeFields $ (fromIntegral (length xs) :: Int64)

    mmsSize _ = mappedVectorSize
    mmsAlignment _ = mappedVectorAlignment
    readFields = MappedList <$> mappedVectorReadFields

instance GVector (List 'Allocated) a where
    glength = length . unAllocatedList
    at = (!!) . unAllocatedList

instance GVector (List 'Mapped) a where
    glength = glength . unMappedList
    at = at . unMappedList
