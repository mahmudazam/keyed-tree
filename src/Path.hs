
module Path
( Path(..)
, IndPair(..)
, PathElem(..)
, pathElemFromStr
, pathFromStr
, toPair
) where

import Str



data IndPair = IndPair { key   :: Str
                       , value :: Str
                       }
                       deriving Show

indPair :: (Str, Str) -> IndPair
indPair pair = IndPair { key=fst pair
                       , value=snd pair
                       }

toPair :: IndPair -> (Str, Str)
toPair ind = (key ind, value ind)

extractIndices :: Str -> [IndPair]
extractIndices str = map toKeyPair indices
    where
        indices = splitWith (== byte ';') str
        toKeyPair p = indPair $ splitOne (byte '=') p



data PathElem = PathElem { node  :: Str
                         , indices :: [IndPair]
                         }
                         deriving Show

pathElemFromStr :: Str -> PathElem
pathElemFromStr str =
    let (nodeName, keys) = splitOne (byte '{') str
        (keys',_) = splitOne (byte '}') keys
    in PathElem { node = nodeName, indices = extractIndices keys'}



type Path = [PathElem]

-- Example Path string:
-- "/node-1/node-2{k21=v21}/node-3{k31=v31;k32=v32}/node-4"

pathFromStr :: Str -> Path
pathFromStr str = map pathElemFromStr (splitWith (== byte '/') str)

