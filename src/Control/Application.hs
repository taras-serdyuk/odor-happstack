{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Application where

import Data.IxSet
import Data.Text
import Control.Monad
import Control.Util
import Happstack.Server
import Model.Item
import Model.Sample
import Model.Sitemap
import Text.Hamlet
import Web.Routes


type RouteHandler = RenderUrl Route -> Route Response
type Route = RouteT Sitemap (ServerPartT IO)


launch :: IO ()
launch = simpleHTTP nullConf $
    msum [site sitemap router, notFoundPage]


itemSet :: IxSet Item
itemSet = fromList sampleItems

router :: Sitemap -> Route Response
router url = renderUrlM >>= case url of
    Home -> homePage
    Overview -> overviewPage sampleItems
    Details itemId -> detailsPage itemId


homePage :: RouteHandler
homePage url = ok $ page "Home" ($(hamletFile (template "Home")) url)

overviewPage :: [Item] -> RouteHandler
overviewPage items url = ok $ page "All Items" ($(hamletFile (template "Overview")) url)

detailsPage :: ItemId -> RouteHandler
detailsPage itemId url = case getOne (itemSet @= itemId) of
    Just item -> ok $ page "Item Details" ($(hamletFile (template "Details")) url)
    Nothing -> notFoundPage

notFoundPage :: (FilterMonad Response m) => m Response
notFoundPage = notFound $ page "Not Found" ($(shamletFile (template "NotFound")))

page :: Text -> Html -> Response
page title content = toResponse $(shamletFile (template "Page"))
