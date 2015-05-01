module Handler.UCD where

import           Control.Lens
import           Data.Aeson.Lens
import           Data.Char (ord)
import qualified Data.Text as T
import           Data.Vector.Lens
import           Import

latest :: UnicodeVersion
latest = Unicode7_0_0

getChar :: UnicodeVersion -> Char -> YesodDB App Character
getChar v c = case v of
  UnicodeLatest -> getChar latest c
  Unicode7_0_0 -> get404 $ CharacterKey (ord c)

getCharById :: UnicodeVersion -> CharacterId -> YesodDB App Character
getCharById v cid = case v of
  UnicodeLatest -> getCharById latest cid
  Unicode7_0_0 -> get404 cid

getCharByHex :: UnicodeVersion -> HexPoint -> YesodDB App Character
getCharByHex v (HexPoint c) = case v of
  UnicodeLatest -> getCharByHex latest (HexPoint c)
  Unicode7_0_0 -> get404 $ CharacterKey c

getCodepointHexR :: UnicodeVersion -> HexPoint -> Handler Value
getCodepointHexR v c =
  toJSON <$> runDB (getCharByHex v c)

getCodepointDecR :: UnicodeVersion -> CharacterId -> Handler Value
getCodepointDecR v cid =
  toJSON <$> runDB (getCharById v cid)

getCharsR :: UnicodeVersion -> Text -> Handler Value
getCharsR v t =
  toJSON <$> runDB (mapM (getChar v) (T.unpack t))

getCharsAttrR :: UnicodeVersion -> Text -> Text -> Handler Value
getCharsAttrR v t a = do
  chars <- runDB $ mapM (getChar v) (T.unpack t)
  return $ _Array # toVectorOf (traverse . to toJSON . key a) chars
