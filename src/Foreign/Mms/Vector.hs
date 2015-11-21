module Foreign.Mms.Vector(Vector(..)) where

class Vector v a where
    length :: v a -> Int
    (!) :: v a -> Int -> a
