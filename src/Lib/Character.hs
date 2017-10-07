module Lib.Character where

import Control.Lens ((.~))
import Data.Bool (Bool(..))
import Data.Int (Int)
import Data.Ratio (Rational)
import Data.Swagger hiding (version, deprecated, name)
import Data.Text (Text)
import Database.Persist.TH

import Lib.Prelude
import Lib.UnicodeVersion

share [ mkPersist sqlSettings { mpsPrefixFields = False }
      , mkMigrate "migrate"
      ]
  [persistLowerCase|
Character json
    version UnicodeVersion
    codePoint Int
    age Text
    name Text
    name1 Text
    block Text
    generalCategory Text
    combiningClass Int
    bidiClass Text
    bidiM Bool
    bidiMirroredGlyph Int Maybe
    bidiControl Bool
    bidiPairedBracketType Text
    bidiPairedBracket Int Maybe
    decompositionType Text
    decomposition [Int]
    compositionExclusion Bool
    compositionExclusionFull Bool
    quickCheckNFC Text
    quickCheckNFD Text
    quickCheckNFKC Text
    quickCheckNFKD Text
    numericType Text
    numericValue Rational Maybe
    joiningType Text
    joiningGroup Text
    joinControl Bool
    linebreak Text
    eastAsianWidth Text
    uppercase Bool
    lowercase Bool
    simpleUppercaseMapping Int Maybe
    simpleLowercaseMapping Int Maybe
    simpleTitlecaseMapping Int Maybe
    uppercaseMapping [Int]
    lowercaseMapping [Int]
    titlecaseMapping [Int]
    caseIgnorable Bool
    cased Bool
    script Text
    scriptExtensions [Text]
    hangulSyllableType Text
    jamoShortName Text
    indicSyllabicCategory Text
    indicMatraCategory Text Maybe
    indicPositionalCategory Text Maybe
    idStart Bool
    xidStart Bool
    idContinue Bool
    xidContinue Bool
    patternSyntax Bool
    patternWhiteSpace Bool
    dash Bool
    quotationMark Bool
    terminalPunctuation Bool
    sentenceTerminal Bool
    diacritic Bool
    extender Bool
    softDotted Bool
    alphabetic Bool
    math Bool
    hexDigit Bool
    asciiHexDigit Bool
    defaultIgnorableCodePoint Bool
    logicalOrderException Bool
    whiteSpace Bool
    graphemeBase Bool
    graphemeExtend Bool
    graphemeClusterBreak Text
    wordBreak Text
    sentenceBreak Text
    ideographic Bool
    unifiedIdeographic Bool
    idsBinaryOperator Bool
    idsTrinaryOperator Bool
    radical Bool
    deprecated Bool
    variationSelector Bool
    noncharacter Bool
    Primary version codePoint
    deriving Eq Show Generic
|]

instance ToSchema Character

-- Orphan instances :(

instance ToSchema (Ratio Integer) where
  declareNamedSchema = pure . NamedSchema Nothing . paramSchemaToSchema

instance ToParamSchema (Ratio Integer) where
  toParamSchema _ = mempty & type_ .~ SwaggerNumber
