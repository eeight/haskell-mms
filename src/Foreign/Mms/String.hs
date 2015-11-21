module Foreign.Mms.String() where

import Foreign.ForeignPtr(newForeignPtr_)
import Foreign.Mms.Class(Mms(..))
import Foreign.Mms.Core(Mode(..))
import Foreign.Mms.MappedVector
import Foreign.Mms.Put(saveOffset, putByteString, zeroPad)
import System.IO.Unsafe(unsafePerformIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

instance Mms B.ByteString B.ByteString where
    writeData str = saveOffset >> putByteString str >> zeroPad 1
    writeFields = mappedVectorWriteFields . fromIntegral . B.length

    mmsSize _ = mappedVectorSize
    mmsAlignment _ = mappedVectorAlignment
    readFields = do
        mv <- mappedVectorReadFields
        case mv of
            MappedVector p l -> let
                p' = unsafePerformIO $ newForeignPtr_ p
                in return $ B.fromForeignPtr p' 0 (fromIntegral l)
