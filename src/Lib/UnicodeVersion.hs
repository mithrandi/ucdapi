module Lib.UnicodeVersion
  ( UnicodeVersion(..)
  ) where

import Control.Lens ((.~), (?~))
import Data.Aeson
import Data.Swagger
import Database.Persist.TH
import GHC.Generics
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

import Lib.Prelude

data UnicodeVersion = UnicodeLatest
                    | Unicode7_0_0
                    | Unicode8_0_0
                    | Unicode9_0_0
                    | Unicode10_0_0
                    deriving (Show, Read, Eq, Ord, Generic, Bounded, Enum)

instance ToJSON UnicodeVersion

instance FromJSON UnicodeVersion

derivePersistField "UnicodeVersion"

instance FromHttpApiData UnicodeVersion where
  parseUrlPiece = \case
    "latest" -> Right UnicodeLatest
    "7.0.0" -> Right Unicode7_0_0
    "8.0.0" -> Right Unicode8_0_0
    "9.0.0" -> Right Unicode9_0_0
    "10.0.0" -> Right Unicode10_0_0
    _ -> Left "Unknown Unicode version"

instance ToHttpApiData UnicodeVersion where
  toUrlPiece = \case
    UnicodeLatest -> "latest"
    Unicode7_0_0 -> "7.0.0"
    Unicode8_0_0 -> "8.0.0"
    Unicode9_0_0 -> "9.0.0"
    Unicode10_0_0 -> "10.0.0"

instance ToSchema UnicodeVersion

instance ToParamSchema UnicodeVersion where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
    & enum_ ?~ map (String . toUrlPiece) [UnicodeLatest .. maxBound]
