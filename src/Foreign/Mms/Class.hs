module Foreign.Mms.Class
    ( Mms(..)
    , GMms(..)
    , Storage(..)
    ) where

import Foreign.Mms.Get(Get(..))
import Foreign.Mms.Internal.Layout(Layout(..), struct, paddings)
import Foreign.Mms.Put(Put(..))
import GHC.Generics(Generic(..))

class Mms a m | a -> m, m -> a where
    writeData :: a -> Put ()
    writeFields :: a -> Put ()

    default writeData ::
        (Generic a, Generic m, GMms (Rep a) (Rep m)) => a -> Put ()
    writeData = gwriteData . from

    default writeFields ::
        (Generic a, Generic m, GMms (Rep a) (Rep m)) => a -> Put ()
    writeFields x = let
        mapped = fakeMapped x
        pads = paddings . struct . gfields . from $ mapped
        in gwriteFields (from x) pads

    mmsSize :: m -> Int
    mmsAlignment :: m -> Int
    readFields :: Get m

    default mmsSize :: (Generic a, Generic m, GMms (Rep a) (Rep m)) => m -> Int
    mmsSize = structSize . struct . gfields . from

    default mmsAlignment ::
        (Generic a, Generic m, GMms (Rep a) (Rep m)) => m -> Int
    mmsAlignment = structAlignment . struct . gfields . from

    default readFields :: (Generic a, Generic m, GMms (Rep a) (Rep m)) => Get m
    readFields = to <$> greadFields

fakeMapped :: Mms a m => a -> m
fakeMapped _ = undefined

class GMms fa fm | fa -> fm, fm -> fa where
    gwriteData :: fa a -> Put ()
    gwriteFields :: fa a -> [Int] -> Put ()
    gfields :: fm a -> [Layout]
    greadFields :: Get (fm a)

class Storage s where
    readMms :: Mms a m => s -> m
