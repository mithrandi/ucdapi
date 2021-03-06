module Handler.UCD where

import           Control.Lens
import           Data.Aeson.Lens
import           Data.Char (ord)
import qualified Data.Text as T
import           Data.Vector.Lens
import           Import
import           UnicodeVersion

getVersionsR :: Handler TypedContent
getVersionsR = selectRep $ do
  let versions :: [UnicodeVersion]
      versions = [minBound..]
  provideRep $ defaultLayout $ do
    cacheSeconds 3600
    setTitle "Available UCD versions"
    $(widgetFile "versions")
  provideRep $ do
    cacheSeconds 3600
    return (_Array # toVectorOf (traverse . to toPathPiece . to toJSON) versions :: Value)

getVersionR :: UnicodeVersion -> Handler Html
getVersionR v = defaultLayout $ do
  cacheSeconds 3600
  setTitle "Available resources"
  let hexpoint = HexPoint 0x2603
      decpoint = 9731
  $(widgetFile "ucd-resources")

getChar :: UnicodeVersion -> Char -> YesodDB App Character
getChar v c = case v of
  UnicodeLatest -> getChar def c
  ver -> get404 $ CharacterKey ver (ord c)

getCharById :: UnicodeVersion -> Int -> YesodDB App Character
getCharById v cid = case v of
  UnicodeLatest -> getCharById def cid
  ver -> get404 $ CharacterKey ver cid

getCharByHex :: UnicodeVersion -> HexPoint -> YesodDB App Character
getCharByHex v (HexPoint c) = case v of
  UnicodeLatest -> getCharByHex def (HexPoint c)
  ver -> get404 $ CharacterKey ver c

getCodepointHexR :: UnicodeVersion -> HexPoint -> Handler Value
getCodepointHexR v c = do
  cacheSeconds 86400
  toJSON <$> runDB (getCharByHex v c)

getCodepointDecR :: UnicodeVersion -> Int -> Handler Value
getCodepointDecR v cid = do
  cacheSeconds 86400
  toJSON <$> runDB (getCharById v cid)

getCharsR :: UnicodeVersion -> Text -> Handler Value
getCharsR v t = do
  cacheSeconds 86400
  toJSON <$> runDB (mapM (getChar v) (T.unpack t))

getCharsAttrR :: UnicodeVersion -> Text -> Text -> Handler Value
getCharsAttrR v t a = do
  cacheSeconds 86400
  chars <- runDB $ mapM (getChar v) (T.unpack t)
  return $ _Array # toVectorOf (traverse . to toJSON . key a) chars
