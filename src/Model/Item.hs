{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Item where

import Data.IxSet
import Data.SafeCopy
import Data.Text
import Data.Typeable
import Text.Blaze


data Item = Item {
    getName :: Name,
    getStatus :: Status,
    getPrice :: Price,
    getDesc :: Description,
    getItemId :: ItemId }
    deriving (Eq, Ord, Typeable)

data Status = Free | Booked deriving (Eq, Ord, Show, Typeable)

newtype ItemId = Id Integer deriving (Eq, Ord, Enum, ToMarkup, Typeable)
newtype Name = Name Text deriving (Eq, Ord, ToMarkup, Typeable)
newtype Price = Price Int deriving (Eq, Ord, ToMarkup, Typeable)
newtype Description = Desc Text deriving (Eq, Ord, ToMarkup)


instance Indexable Item where
    empty = ixSet [
        ixFun1 getItemId,
        ixFun1 getName,
        ixFun1 getStatus,
        ixFun1 getPrice ] where
        ixFun1 f = ixFun $ return . f


setStatus :: Status -> Item -> Item
setStatus status item = item { getStatus = status }


$(deriveSafeCopy 0 'base ''Item)
$(deriveSafeCopy 0 'base ''ItemId)
$(deriveSafeCopy 0 'base ''Name)
$(deriveSafeCopy 0 'base ''Status)
$(deriveSafeCopy 0 'base ''Price)
$(deriveSafeCopy 0 'base ''Description)
