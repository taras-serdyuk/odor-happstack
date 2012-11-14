{-# LANGUAGE OverloadedStrings #-}

module Model.Sample where

import Model.IdSet
import Model.Item


initItems :: IdSet Item ItemId
initItems = foldr put (empty (Id 0)) [
    Item (Name "Item A") Free (Desc "Some description."),
    Item (Name "Item B") Free (Desc "Some description."),
    Item (Name "Item C") Free (Desc "Some description."),
    Item (Name "Item D") Booked (Desc "Some description.")]
