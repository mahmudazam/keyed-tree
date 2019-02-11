
module KeyedTree
( KeyedTree(..)
, Key
, KeyPath
, exampleTree
, get
, getWithKeyPath
, getWithPath
, keyPathFromPath
) where

import Data.HashMap.Strict as HM

import Path
import Str

type Key = HashMap Str Str
type KeyPath = [Key]

keyPathFromPath :: Path -> KeyPath
keyPathFromPath path = Prelude.map toKey path
    where
        toKey pathElem =
            let nodeKey = (b "node", node pathElem)
                keys = Prelude.map toPair (indices pathElem)
            in fromList (nodeKey:keys)



data KeyedTree = Leaf Str
    | Internal (HashMap Key KeyedTree) -- Children: [key:val] -> child
    deriving Show

get :: KeyedTree -> Str -> Maybe KeyedTree
get tree pathStr = getWithPath tree actualPath
    where
        root = PathElem { node = b "/", indices = [] }
        actualPath = root:(pathFromStr pathStr)

getWithPath :: KeyedTree -> Path -> Maybe KeyedTree
getWithPath tree path = getWithKeyPath tree (keyPathFromPath path)

getWithKeyPath :: KeyedTree -> KeyPath -> Maybe KeyedTree
getWithKeyPath a [] = Just a
getWithKeyPath (Internal children) (k : path') = do
    next <- HM.lookup k children
    getWithKeyPath next path'

exampleTree :: KeyedTree
-- /
-- |-- a
--     |-- aa
--         |-- aaval
--
--     |-- ab
--         |-- abval
-- |-- b
--     |-- ba
--         |-- baval
--
--     |-- bb
--         |-- bbval
exampleTree = Internal $ fromList $ [
        (
            fromList [(b "node", b "/")],
            Internal $ fromList $ [
                (
                    fromList [(b "node", b "a")],
                    Internal $ fromList $ [
                        (
                            fromList [(b "node", b "aa"), (b "k1", b "v1")],
                            Internal $ fromList [
                                (
                                    fromList [(b "node", b "aaa")],
                                    Leaf $ b "aaaval"
                                )
                            ]
                        ),
                        (
                            fromList [(b "node", b "ab")],
                            Leaf $ b "abval"
                        )
                    ]
                ),
                (
                    fromList [(b "node", b "b")],
                    Internal $ fromList $ [
                        (
                            fromList [(b "node", b "ba")],
                            Leaf $ b "baval"
                        ),
                        (
                            fromList [(b "node", b "bb")],
                            Leaf $ b "bbval"
                        )
                    ]
                )
            ]
        )
    ]

