{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Application where

import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Local
import Data.Text
import Control.Actions
import Control.Exception
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
launch = bracket (openLocalStateFrom "../state" initItems) createCheckpointAndClose
    (\items -> simpleHTTP nullConf $ msum
    [site sitemap (router items), notFoundPage])


router :: AcidState Items -> Sitemap -> Route Response
router items url = renderUrlM >>= case url of
    Home -> homePage
    Overview -> overviewPage items
    Details itemId -> detailsPage $ query' items (GetItem itemId)
    Book itemId -> detailsPage $ update' items (BookItem itemId)


homePage :: RouteHandler
homePage url = ok $ page "Home" ($(hamletFile (template "Home")) url)

overviewPage :: AcidState Items -> RouteHandler
overviewPage items' url = do
    items <- query' items' GetItems
    ok $ page "All Items" ($(hamletFile (template "Overview")) url)

detailsPage :: Route (Maybe Item) -> RouteHandler
detailsPage action url = do
    item' <- action
    case item' of
        Just item -> ok $ page "Item Details" ($(hamletFile (template "Details")) url)
        Nothing -> notFoundPage

notFoundPage :: (FilterMonad Response m) => m Response
notFoundPage = notFound $ page "Not Found" ($(shamletFile (template "NotFound")))

page :: Text -> Html -> Response
page title content = toResponse $(shamletFile (template "Page"))
