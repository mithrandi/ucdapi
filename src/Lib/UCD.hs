module Lib.UCD (
  server
  , AppM
  , ServerM
  ) where

import           Control.Lens hiding ((&))
import           Data.Aeson
import           Data.HashMap.Lazy (lookup)
import           Data.Swagger
import qualified Database.Persist as P
import           Database.Persist.Sqlite (SqlPersistT)
import           Network.Wai.Middleware.Prometheus (metricsApp)
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI (swaggerSchemaUIServer)

import           Lib.API (API, UnicodeAPI, UCDAPI, CharsAPI, CharAttr(..))
import           Lib.Character (Character, Key(CharacterKey))
import           Lib.Prelude
import           Lib.UnicodeVersion

type AppM = SqlPersistT Handler

type ServerM a = ServerT a AppM

-- Orphan instance, ick
instance ToSchema a => ToSchema (Identity a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

serveChars :: forall f . Traversable f => UnicodeVersion -> f Int -> ServerM (CharsAPI f)
serveChars ver cs = char :<|> attr
  where
    getChar :: Int -> AppM Character
    getChar c = maybe (throwError err404) return =<< P.get (CharacterKey ver c)
    char :: AppM (f Character)
    char = traverse getChar cs
    attr :: Text -> AppM (f CharAttr)
    attr a = traverse (getAttr a) =<< char
    getAttr :: Text -> Character -> AppM CharAttr
    getAttr a c = do
      enc <- case toJSON c of
        Object o -> return o
        _ -> throwError err404
      case lookup a enc of
        Just val -> return (CharAttr val)
        _ -> throwError err404

serveUnicode :: UnicodeVersion -> ServerM UnicodeAPI
serveUnicode ver = char :<|> char :<|> chars
  where
    ver' :: UnicodeVersion
    ver' = case ver of
      UnicodeLatest -> maxBound
      v -> v
    char = serveChars ver' . Identity . fromIntegral
    chars = serveChars ver' . map ord . toS

ucdSwagger :: Swagger
ucdSwagger = toSwagger (Proxy :: Proxy UCDAPI)
  & info.title   .~ "UCDAPI"
  & info.version .~ "1.0"
  & info.description ?~ "An API for accessing the Unicode Character Database"
  & info.license ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")

server :: ServerM API
server = (serveVersions :<|> serveUnicode)
  :<|> Tagged metricsApp
  :<|> enter nt (swaggerSchemaUIServer ucdSwagger)
  where nt :: Handler :~> AppM
        nt = NT lift
        serveVersions = return $ map toUrlPiece [UnicodeLatest .. maxBound]
