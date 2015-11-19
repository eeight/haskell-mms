module Foreign.Mms.Class
    ( Mms(..)
    , GMms(..)
    , Storage(..)
    ) where

import Control.Monad.State.Strict(StateT, evalStateT)
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
        fakeMapped :: Mms a m => a -> m
        fakeMapped _ = undefined

        mapped = fakeMapped x
        pads = paddings . struct . gfields . from $ mapped
        in evalStateT (gwriteFields (from x)) pads

    mmsSize :: m -> Int
    mmsAlignment :: m -> Int
    readFields :: Get m

    default mmsSize :: (Generic a, Generic m, GMms (Rep a) (Rep m)) => m -> Int
    mmsSize = structSize . struct . gfields . from

    default mmsAlignment ::
        (Generic a, Generic m, GMms (Rep a) (Rep m)) => m -> Int
    mmsAlignment = structAlignment . struct . gfields . from

    default readFields :: (Generic a, Generic m, GMms (Rep a) (Rep m)) => Get m
    readFields = let
        fakeUnget :: Get a -> a
        fakeUnget _ = undefined

        pads = paddings . struct . gfields . from . fakeUnget $ result
        result = to <$> evalStateT greadFields pads
        in result

class GMms fa fm | fa -> fm, fm -> fa where
    gwriteData :: fa a -> Put ()
    gwriteFields :: fa a -> StateT [Int] Put ()
    gfields :: fm a -> [Layout]
    greadFields :: StateT [Int] Get (fm a)

class Storage s where
    readMms :: Mms a m => s -> m
