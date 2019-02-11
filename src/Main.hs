module Main where

import Data.HashMap.Strict

import KeyedTree
import Path
import Str

main :: IO ()
main = do
    putStrLn $ show $ get exampleTree (b "/a/aa{k1=v1}/aaa")
    putStrLn $ show $ get exampleTree (b "/a/aa{k2=v2}/aaa2")
    putStrLn $ show $ get exampleTree (b "/b/ba")

