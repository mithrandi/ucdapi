module Lib.API (
  API
  , UCDAPI
  , UnicodeAPI
  , CharsAPI
  , CharAttr(..)
  , Hex(..)
  ) where

import Control.Lens ((.~))
import Data.Aeson (Value, ToJSON, FromJSON)
import Data.Swagger
import Data.Text.Read (hexadecimal)
import Numeric (showHex)
import Servant.API
import Servant.Swagger.UI
import Text.Show (Show(..), showParen, showString)
import Web.Internal.HttpApiData (parseBounded)

import Lib.Character
import Lib.Prelude
import Lib.UnicodeVersion (UnicodeVersion)

newtype Hex = Hex Int
  deriving (Eq, Ord, Bounded, Num, Integral, Real, Enum, Read)

instance Show Hex where
  showsPrec p (Hex n) = showParen (p > 10) $ showString "Hex 0x" . showHex n

instance FromHttpApiData Hex where
  parseUrlPiece = parseBounded hexadecimal

instance ToParamSchema Hex where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString

newtype CharAttr = CharAttr Value
  deriving (Eq, Show, FromJSON, ToJSON)

instance ToSchema CharAttr where
  declareNamedSchema _ = pure $ NamedSchema (Just "Character attribute") mempty

type API = UCDAPI
           :<|> "metrics" :> Raw
           :<|> SwaggerSchemaUI "foo-ui" "swagger.json"

type UCDAPI = "unicode" :> Capture "version" UnicodeVersion :> UnicodeAPI

type UnicodeAPI =
  "codepoint" :> "hex" :> Capture "codepoint" Hex :> CharsAPI Identity
  :<|> "codepoint" :> "dec" :> Capture "codepoint" Int :> CharsAPI Identity
  :<|> "chars" :> Capture "chars" Text :> CharsAPI []

type CharsAPI f =
  Get '[JSON] (f Character)
  :<|> Capture "attr" Text :> Get '[JSON] (f CharAttr)
