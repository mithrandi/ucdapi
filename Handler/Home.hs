module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  cacheSeconds 86400
  setTitle "Unicode Character Database HTTP API"
  $(widgetFile "homepage")
