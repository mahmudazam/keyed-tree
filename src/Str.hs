
module Str
( Str
, b
, splitOne
, splitWith
, byte
, len
, empty
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word

type Str = B.ByteString

b :: [Char] -> Str
b = C.pack

byte :: Char -> Word8
byte c = B.head $ b [c]

splitWith fn = (filter (not . empty)) . (B.splitWith fn)

splitOne :: Word8 -> Str -> (Str, Str)
splitOne c str =
    let (k, v') = B.span (not . (== c)) str
    in (k, B.drop 1 v')

len :: Str -> Int
len = B.length

empty :: Str -> Bool
empty = B.null

