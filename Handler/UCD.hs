module Handler.UCD where

import           Control.Lens
import           Data.Char (ord)
import qualified Data.Text as T
import           Import

getCodepointR :: CharacterId -> Handler Value
getCodepointR cid =
  toJSON <$> runDB (get404 cid)

getChar :: Char -> YesodDB App Character
getChar c = get404 $ CharacterKey (ord c)

getCharR :: Text -> Handler Value
getCharR t =
  toJSON <$> runDB (getChar . T.head $ t)

getNamesR :: Text -> Handler Value
getNamesR t = do
  chars <- runDB $ mapM getChar (T.unpack t)
  return . toJSON $ chars ^.. traverse . name
