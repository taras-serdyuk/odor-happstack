{-# LANGUAGE OverloadedStrings #-}

module Model.Sample where

import Model.IdSet
import Model.Item


initItems :: IdSet Item ItemId
initItems = foldr put (empty (Id 0)) [
    Item (Name "Item A") Free (Price 40) (Desc "Some description."),
    Item (Name "Item B") Free (Price 20) (Desc "Some description."),
    Item (Name "Item C") Free (Price 30) (Desc "Some description."),
    Item (Name "Item D") Booked (Price 50) (Desc "Some description.")]
