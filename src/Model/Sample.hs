{-# LANGUAGE OverloadedStrings #-}

module Model.Sample where

import Model.Item


sampleItems :: [Item]
sampleItems = [
    Item (Id 1) (Name "Item A") Free (Desc "Some description."),
    Item (Id 2) (Name "Item B") Free (Desc "Some description."),
    Item (Id 3) (Name "Item C") Free (Desc "Some description."),
    Item (Id 4) (Name "Item D") Booked (Desc "Some description.")]
