module Foreign.Mms.MappedVector(MappedVector) where

import Foreign.Mms.GVector(GVector(..))
import Foreign.Mms.Core(FromMms(..), Storage(..), getPointer, getStorable)
import Foreign.Ptr(Ptr, plusPtr)
import GHC.Int(Int64)
import Control.Monad(liftM2)
import Data.Foldable(Foldable(..))

data MappedVector a where
    MappedVector :: FromMms a => Ptr a -> Int64 -> MappedVector a

instance FromMms a => FromMms (MappedVector a) where
    mmsSize _ = 16
    readFields = liftM2 MappedVector getPointer getStorable

instance GVector MappedVector a where
    glength (MappedVector _ length) = fromIntegral length
    at (MappedVector p length) index = let
        size = mmsSize result
        result = readMms (p `plusPtr` (size * index))
        in result

instance Show a => Show (MappedVector a) where
    showsPrec p x = showParen (p > 10) $
        showString "MappedVector ". shows (toList x)

instance Foldable MappedVector where
    toList (MappedVector p length) = let
        xs = take (fromIntegral length) $ map readMms $
            iterate (`plusPtr` elementSize) p
        elementSize = mmsSize $ head xs
        in xs

    foldr f z v = foldr f z (toList v)
