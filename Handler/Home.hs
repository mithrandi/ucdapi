module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Unicode Character Database HTTP API"
  $(widgetFile "homepage")
