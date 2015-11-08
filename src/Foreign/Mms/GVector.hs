module Foreign.Mms.GVector(GVector(..)) where

class GVector v a where
    glength :: v a -> Int
    at :: v a -> Int -> a
