module Foreign.Mms.Class
    ( ToMms(..)
    , FromMms(..)
    , GToMms(..)
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

class GToMms f where
    gwriteData :: f a -> Put ()
    gwriteFields :: f a -> Put ()

class Storage a where
    readMms :: FromMms b => a -> b
