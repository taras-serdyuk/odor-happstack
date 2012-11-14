{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Control.Actions where

import Control.Monad.Reader
import qualified Control.Monad.State as St
import Data.Acid hiding (update)
import Data.Typeable
import Model.IdSet
import Model.Item


type Items = IdSet Item ItemId


getItems :: Query Items [Item]
getItems = fmap (toList . ixSet) ask

getItem :: ItemId -> Query Items (Maybe Item)
getItem i = fmap (get i) ask

bookItem :: ItemId -> Update Items (Maybe Item)
bookItem = updateById (setStatus Booked)


updateById :: (Indexable a, Ord a, Typeable a, Typeable id) =>
    (a -> a) -> id -> Update (IdSet a id) (Maybe a)
updateById f i = do
    set <- St.get
    case fmap f (get i set) of
        Just x -> St.put (update i x set) >> return (Just x)
        Nothing -> return Nothing


$(makeAcidic ''Items ['getItems, 'getItem, 'bookItem])
