{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Model.Sitemap where

import Model.Item
import Text.Boomerang.TH
import Web.Routes.Boomerang
import Prelude hiding ((.))
import Control.Category ((.))


data Sitemap = Home | Overview | Details ItemId | Book ItemId

$(derivePrinterParsers ''Sitemap)
$(derivePrinterParsers ''ItemId)


sitemap :: Router () (Sitemap :- ())
sitemap =
    rHome <>
    rOverview . "items" <>
    rDetails . "item" </> rId . integer <>
    rBook . "book" </> rId . integer
