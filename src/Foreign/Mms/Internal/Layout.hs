module Foreign.Mms.Internal.Layout(Layout(..), builtin, struct) where

import Data.Foldable(foldl')

data Layout = Layout
    { structAlignment :: Int
    , structSize :: Int
    , structFields :: [(Layout, Int)]
    } deriving (Show, Eq)

builtin :: Int -> Layout
builtin size = Layout size size []

padding :: Int -> Int -> Int
padding pos alignment = (alignment - pos `mod` alignment) `mod` alignment

layout pos (f:g:fs) = let
    fEnd = pos + structSize f
    pad = padding fEnd $ structAlignment g
    in pad:layout (fEnd + pad) (g:fs)
layout _ _ = []

struct :: [Layout] -> Layout
struct fields = let
    alignment = maximum (map structAlignment fields)
    pads = layout 0 (fields ++ [result])
    size = sum pads + sum (map structSize fields)
    result = Layout alignment size $ zip fields pads
    in result
