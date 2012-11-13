{-# LANGUAGE TypeOperators #-}

module Control.Util where

import Control.Arrow
import Control.Monad
import Data.Text hiding (map)
import Happstack.Server
import Web.Routes
import Web.Routes.Boomerang
import Web.Routes.Happstack


type RenderUrl m = URL m -> [(Text, Text)] -> Text


site :: (Functor m, Monad m, MonadPlus m, ServerMonad m) =>
    Router () (url :- ()) -> (url -> RouteT url m a) -> m a
site sitemap router = implSite empty empty $
    boomerangSite (runRouteT router) sitemap

template :: String -> String
template name = "src/View/" ++ name ++ ".hamlet"

renderUrlM :: (MonadRoute m) => m (RenderUrl m)
renderUrlM = liftM (\f url -> f url . map (second Just)) askRouteFn
