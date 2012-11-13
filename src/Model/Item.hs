
module Model.Item where

import Data.Text


data Item = Item ItemId Name Status Description

data Status = Free | Booked

newtype ItemId = Id { unId :: Int }
newtype Name = Name { unName :: Text }
newtype Description = Desc { unDesc :: Text }
