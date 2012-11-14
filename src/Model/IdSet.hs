{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.IdSet (
    IdSet (IdSet), ixSet, empty,
    get, put, update,
    module Data.IxSet) where

import Data.IxSet hiding (empty, ixSet)
import qualified Data.IxSet as S
import Data.SafeCopy
import Data.Typeable


-- http://code.google.com/p/happstack/issues/detail?id=229
data (Indexable a, Ord a, Typeable a, Typeable id) =>
    IdSet a id = IdSet (IxSet a) id deriving (Typeable)


ixSet :: (Indexable a, Ord a, Typeable a, Typeable id) =>
    IdSet a id -> IxSet a
ixSet (IdSet set _) = set

empty :: (Indexable a, Ord a, Typeable a, Typeable id) =>
    id -> IdSet a id
empty = IdSet S.empty

get :: (Indexable a, Ord a, Typeable a, Typeable id) =>
    id -> IdSet a id -> Maybe a
get i (IdSet set _) = getOne (set @= i)

put :: (Indexable a, Ord a, Typeable a, Typeable id, Enum id) =>
    (id -> a) -> IdSet a id -> IdSet a id
put new (IdSet set i) = IdSet (updateIx i (new i) set) (succ i)

update :: (Indexable a, Ord a, Typeable a, Typeable id) =>
    id -> a -> IdSet a id -> IdSet a id
update i x (IdSet set ni) = IdSet (updateIx i x set) ni


$(deriveSafeCopy 0 'base ''IdSet)
