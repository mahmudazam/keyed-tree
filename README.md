# keyed-tree
A keyed tree datastore

### Overview
An in-memory datastore for hierarchical data. This is an infinitary tree as defined below

```BNF
tree = tree label children
    with
    label = optional bytes
    children = [(bytes, bytes)] -> tree
```

A typical path in the tree is `/a/b/{b1=v1;b2=v2}/c`

### Run
```
cabal run
```
