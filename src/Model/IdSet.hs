{-# LANGUAGE MultiParamTypeClasses #-}

module Model.IdSet (
    IdSet, ixSet, empty, get, put,
    module Data.IxSet) where

import Data.IxSet hiding (empty, ixSet)
import qualified Data.IxSet as S
import Data.Typeable


data IdSet a id = IdSet (IxSet a) id


ixSet :: IdSet a id -> IxSet a
ixSet (IdSet set _) = set

empty :: (Indexable a) => id -> IdSet a id
empty = IdSet S.empty

get :: (Indexable a, Ord a, Typeable a, Typeable id) =>
    id -> IdSet a id -> Maybe a
get id' (IdSet set _) = getOne (set @= id')

put :: (Indexable a, Ord a, Typeable a, Typeable id, Enum id) =>
    (id -> a) -> IdSet a id -> IdSet a id
put new (IdSet set id') = IdSet (updateIx id' (new id') set) (succ id')
