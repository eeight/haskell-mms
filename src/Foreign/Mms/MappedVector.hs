module Foreign.Mms.MappedVector
    ( MappedVector
    , mappedVectorSize
    , mappedVectorAlignment
    , mappedVectorReadFields
    ) where

import Control.Monad(liftM2)
import Data.Foldable(Foldable(..))
import Foreign.Mms.Class(Mms(..), Storage(..))
import Foreign.Mms.GVector(GVector(..))
import Foreign.Mms.Get(Get, getPointer)
import Foreign.Mms.Instances
import Foreign.Ptr(Ptr, plusPtr)
import GHC.Int(Int64)

data MappedVector a where
    MappedVector :: Mms a m => Ptr m -> Int64 -> MappedVector m

mappedVectorSize :: Int
mappedVectorSize = 16

mappedVectorAlignment :: Int
mappedVectorAlignment = 8

mappedVectorReadFields :: Mms a m => Get (MappedVector m)
mappedVectorReadFields = liftM2 MappedVector getPointer readFields

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
        elementSize = mmsSize . head $ xs
        in xs

    foldr f z v = foldr f z (toList v)
