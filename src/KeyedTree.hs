
module KeyedTree
( KeyedTree(..)
, Key
, KeyPath
, exampleTree
, get
, set
) where

import Data.HashMap.Strict as HM

import Path
import Str



data KeyedTree = KeyedTree { label :: (Maybe Str)
                           , children :: (HashMap Key KeyedTree)
                           }
                           deriving Show

get :: KeyedTree -> Str -> Maybe KeyedTree
get tree pathStr = getWithKeyPath tree (rootPath pathStr)

getWithKeyPath :: KeyedTree -> KeyPath -> Maybe KeyedTree
getWithKeyPath a [] = Just a
getWithKeyPath (KeyedTree { label = _, children = c }) (k : path') = do
    next <- HM.lookup k c
    getWithKeyPath next path'

set :: KeyedTree -> Str -> KeyedTree -> KeyedTree
set tree pathStr toInsert = setWithKeyPath tree (rootPath pathStr) toInsert

setWithKeyPath :: KeyedTree -> KeyPath -> KeyedTree -> KeyedTree
setWithKeyPath _ [] toInsert = toInsert
setWithKeyPath (KeyedTree { label = v, children = c }) (k:path') toInsert =
    KeyedTree { label = v
              , children = insert k child c
              }
        where
            child = if member k c
                then setWithKeyPath (c ! k) path' toInsert
                else let newChild = KeyedTree { label = Nothing
                                              , children = fromList []
                                              }
                     in setWithKeyPath newChild path' toInsert



type Key = HashMap Str Str
type KeyPath = [Key]

keyPathFromPath :: Path -> KeyPath
keyPathFromPath path = Prelude.map toKey path
    where
        toKey pathElem =
            let nodeKey = (b "node", node pathElem)
                keys = Prelude.map toPair (indices pathElem)
            in fromList (nodeKey:keys)

rootPath :: Str -> KeyPath
rootPath pathStr = keyPathFromPath $ root:(pathFromStr pathStr)
    where
        root = PathElem { node = b "/", indices = [] }



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
exampleTree = KeyedTree Nothing $ fromList [
        (
            fromList [(b "node", b "/")],
            KeyedTree Nothing $ fromList [
                (
                    fromList [(b "node", b "a")],
                    KeyedTree Nothing $ fromList [
                        (
                            fromList [(b "node", b "aa"), (b "k1", b "v1")],
                            KeyedTree Nothing $ fromList [
                                (
                                    fromList [(b "node", b "aaa")],
                                    KeyedTree (Just $ b "aaaval") $ fromList []
                                )
                            ]
                        ),
                        (
                            fromList [(b "node", b "ab")],
                            KeyedTree (Just $ b "abval") $ fromList []
                        )
                    ]
                ),
                (
                    fromList [(b "node", b "b")],
                    KeyedTree Nothing $ fromList [
                        (
                            fromList [(b "node", b "ba")],
                            KeyedTree (Just $ b "baval") $ fromList []
                        ),
                        (
                            fromList [(b "node", b "bb")],
                            KeyedTree (Just $ b "bbval") $ fromList []
                        )
                    ]
                )
            ]
        )
    ]

