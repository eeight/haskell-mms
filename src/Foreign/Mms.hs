module Foreign.Mms where

import Data.Monoid((<>))
import Foreign.Mms.Builder(Builder, storable, toLazyByteString)
import Foreign.Ptr
import Foreign.Storable

import qualified Data.ByteString.Lazy as L

data PutM a = PutM a Builder
type Put = PutM ()

runPut :: Put -> L.ByteString
runPut (PutM () b) = toLazyByteString b

put :: Builder -> Put
put = PutM ()

instance Functor PutM where
    fmap f (PutM x b) = PutM (f x) b

instance Applicative PutM where
    pure x = PutM x mempty
    (PutM f b1) <*> (PutM x b2) = PutM (f x) (b1 <> b2)

instance Monad PutM where
    (PutM x b1) >>= f = case f x of PutM y b2 -> PutM y (b1 <> b2)

class Mms m where
    type Freeze m :: *
    writeFields :: m -> Put

instance Mms Double where
    type Freeze Double = Double
    writeFields = put . storable

data Point = Point Double Double

instance Mms Point where
    type Freeze Point = Point
    writeFields (Point x y) = mapM_ writeFields [x, y]
