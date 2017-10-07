{-# LANGUAGE NoMonoPatBinds #-}
module Lib.Load (loadUCD) where

import           Control.Lens
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Lens (_Text)
import           Database.Persist
import           Database.Persist.Sql (SqlBackend)
import           Numeric.Lens (decimal, hex)
import           Text.XML.Expat.SAX

import           Lib.Character
import           Lib.Prelude hiding (to)
import           Lib.UnicodeVersion

yn :: (IsString a, Eq a) => Prism' a Bool
yn = prism (bool "N" "Y") $ \case
  "N" -> Right False
  "Y" -> Right True
  x -> Left x
{-# INLINE yn #-}

expandName :: Int -> Text -> Text
expandName cp = T.replace "#" (T.justifyRight 4 '0' $ _Text . hex # cp)
{-# INLINE expandName #-}

plainCP :: Prism' Text Int
plainCP = _Text . hex
{-# INLINE plainCP #-}

expandCP :: Int -> Prism' Text Int
expandCP cp = prism' toCP fromCP
  where toCP c | c == cp = "#"
               | otherwise = _Text . hex # c
        fromCP "#" = Just cp
        fromCP t = t ^? _Text . hex
{-# INLINE expandCP #-}

tworded :: Traversal' Text Text
tworded f = fmap T.unwords . traverse f . T.words
{-# INLINE tworded #-}

insertChar :: (Applicative m, MonadIO m)
             => UnicodeVersion
             -> M.Map Text Text
             -> Int
             -> ReaderT SqlBackend m ()
insertChar ver x cp = do
  _ <- insert Character
    { version = ver
    , codePoint = cp
    , age = x^?!ix "age"
    , name = x^?!ix "na".name'
    , name1 = x^?!ix "na1"
    , block = x^?!ix "blk"
    , generalCategory = x^?!ix "gc"
    , combiningClass = x^?!ix "ccc"._Text.decimal
    , bidiClass = x^?!ix "bc"
    , bidiM = x^?!ix "Bidi_M".yn
    , bidiMirroredGlyph = x^?ix "bmg".cp'
    , bidiControl = x^?!ix "Bidi_C".yn
    , bidiPairedBracketType = x^?!ix "bpt"
    , bidiPairedBracket = x^?ix "bpb".cp'
    , decompositionType = x^?!ix "dt"
    , decomposition = x^..ix "dm".tworded.cp'
    , compositionExclusion = x^?!ix "CE".yn
    , compositionExclusionFull = x^?!ix "Comp_Ex".yn
    , quickCheckNFC = x^?!ix "NFC_QC"
    , quickCheckNFD = x^?!ix "NFD_QC"
    , quickCheckNFKC = x^?!ix "NFKC_QC"
    , quickCheckNFKD = x^?!ix "NFKD_QC"
    , numericType = x^?!ix "nt"
    , numericValue = x^?ix "nv".to (T.replace "/" "%")._Text._Show
    , joiningType = x^?!ix "jt"
    , joiningGroup = x^?!ix "jg"
    , joinControl = x^?!ix "Join_C".yn
    , linebreak = x^?!ix "lb"
    , eastAsianWidth = x^?!ix "ea"
    , uppercase = x^?!ix "Upper".yn
    , lowercase = x^?!ix "Lower".yn
    , simpleUppercaseMapping = x^?ix "suc".cp'
    , simpleLowercaseMapping = x^?ix "slc".cp'
    , simpleTitlecaseMapping = x^?ix "stc".cp'
    , uppercaseMapping = x^..ix "uc".tworded.cp'
    , lowercaseMapping = x^..ix "lc".tworded.cp'
    , titlecaseMapping = x^..ix "tc".tworded.cp'
    , caseIgnorable = x^?!ix "CI".yn
    , cased = x^?!ix "Cased".yn
    , script = x^?!ix "sc"
    , scriptExtensions = x^..ix "scx".tworded
    , hangulSyllableType = x^?!ix "hst"
    , jamoShortName = x^?!ix "JSN"
    , indicSyllabicCategory = x^?!ix "InSC"
    , indicMatraCategory = x^?ix "InMC"
    , indicPositionalCategory = x^?ix "InPC"
    , idStart = x^?!ix "IDS".yn
    , xidStart = x^?!ix "XIDS".yn
    , idContinue = x^?!ix "IDC".yn
    , xidContinue = x^?!ix "XIDC".yn
    , patternSyntax = x^?!ix "Pat_Syn".yn
    , patternWhiteSpace = x^?!ix "Pat_WS".yn
    , dash = x^?!ix "Dash".yn
    , quotationMark = x^?!ix "QMark".yn
    , terminalPunctuation = x^?!ix "Term".yn
    , sentenceTerminal = x^?!ix "STerm".yn
    , diacritic = x^?!ix "Dia".yn
    , extender = x^?!ix "Ext".yn
    , softDotted = x^?!ix "SD".yn
    , alphabetic = x^?!ix "Alpha".yn
    , math = x^?!ix "Math".yn
    , hexDigit = x^?!ix "Hex".yn
    , asciiHexDigit = x^?!ix "AHex".yn
    , defaultIgnorableCodePoint = x^?!ix "DI".yn
    , logicalOrderException = x^?!ix "LOE".yn
    , whiteSpace = x^?!ix "WSpace".yn
    , graphemeBase = x^?!ix "Gr_Base".yn
    , graphemeExtend = x^?!ix "Gr_Ext".yn
    , graphemeClusterBreak = x^?!ix "GCB"
    , wordBreak = x^?!ix "WB"
    , sentenceBreak = x^?!ix "SB"
    , ideographic = x^?!ix "Ideo".yn
    , unifiedIdeographic = x^?!ix "UIdeo".yn
    , idsBinaryOperator = x^?!ix "IDSB".yn
    , idsTrinaryOperator = x^?!ix "IDST".yn
    , radical = x^?!ix "Radical".yn
    , deprecated = x^?!ix "Dep".yn
    , variationSelector = x^?!ix "VS".yn
    , noncharacter = x^?!ix "NChar".yn
    }
  return ()
  where name' = to (expandName cp)
        cp' :: Prism' Text Int
        cp' = expandCP (cp :: Int)
{-# INLINE insertChar #-}

loadUCD :: (Applicative m, MonadIO m) =>
  UnicodeVersion
  -> Text
  -> ReaderT SqlBackend m ()
loadUCD ver inputFileName = do
  deleteWhere [Version ==. ver]
  content <- liftIO $ L.readFile (T.unpack inputFileName)
  let xml = parseThrowing defaultParseOptions content
  forM_ [M.fromList as | StartElement "char" as <- xml] $ \x ->
    case x ^? ix "cp" . plainCP of
     Just cp -> insertChar ver x cp
     Nothing -> do
       let Just firstChar = x ^? ix "first-cp" . plainCP
           Just lastChar = x ^? ix "last-cp" . plainCP
       mapM_ (insertChar ver x) [firstChar..lastChar]
