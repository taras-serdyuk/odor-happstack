{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Item where

import Data.IxSet
import Data.Text
import Data.Typeable
import Text.Blaze


data Item = Item {
    getItemId :: ItemId,
    getName :: Name,
    getStatus :: Status,
    getDesc :: Description }
    deriving (Eq, Ord, Typeable)

data Status = Free | Booked deriving (Eq, Ord, Show, Typeable)

newtype ItemId = Id Integer deriving (Eq, Ord, ToMarkup, Typeable)
newtype Name = Name Text deriving (Eq, Ord, ToMarkup, Typeable)
newtype Description = Desc Text deriving (Eq, Ord, ToMarkup)


instance Indexable Item where
    empty = ixSet [
        ixFun1 getItemId,
        ixFun1 getName,
        ixFun1 getStatus ] where
        ixFun1 f = ixFun $ return . f
