module Foreign.Mms.Class
    ( ToMms(..)
    , FromMms(..)
    , GToMms(..)
    , GFromMms(..)
    , Storage(..)
    ) where

import Foreign.Mms.Get(Get(..))
import Foreign.Mms.Put(Put(..))
import GHC.Generics(Generic(..))

class ToMms a where
    writeData :: a -> Put ()
    writeFields :: a -> Put ()

    default writeData :: (Generic a, GToMms (Rep a)) => a -> Put ()
    writeData = gwriteData . from

    default writeFields :: (Generic a, GToMms (Rep a)) => a -> Put ()
    writeFields = gwriteFields . from

class FromMms a where
    mmsSize :: a -> Int
    readFields :: Get a

    default mmsSize :: (Generic a, GFromMms (Rep a)) => a -> Int
    mmsSize = gmmsSize . from

    default readFields :: (Generic a, GFromMms (Rep a)) => Get a
    readFields = to <$> greadFields

class GToMms f where
    gwriteData :: f a -> Put ()
    gwriteFields :: f a -> Put ()

class GFromMms f where
    gmmsSize :: f a -> Int
    greadFields :: Get (f a)

class Storage a where
    readMms :: FromMms b => a -> b
