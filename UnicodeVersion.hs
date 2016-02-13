module UnicodeVersion where

import Data.Aeson
import Data.Default (Default(..))
import GHC.Generics
import Prelude
import Yesod


data UnicodeVersion = UnicodeLatest
                    | Unicode7_0_0
                    | Unicode8_0_0
                    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON UnicodeVersion

instance FromJSON UnicodeVersion

derivePersistField "UnicodeVersion"

instance PathPiece UnicodeVersion where
  toPathPiece = \case
    UnicodeLatest -> "latest"
    Unicode7_0_0 -> "7.0.0"
    Unicode8_0_0 -> "8.0.0"
  fromPathPiece = \case
    "latest" -> Just UnicodeLatest
    "7.0.0" -> Just Unicode7_0_0
    "8.0.0" -> Just Unicode8_0_0
    _ -> Nothing

instance Default UnicodeVersion where
  def = Unicode8_0_0
