{-# LANGUAGE NoMonoPatBinds #-}
module Load (loadUCD) where

import           Control.Lens hiding (children)
import           Data.Bool.Extras (bool)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Lens (_Text)
import           Import
import           Numeric.Lens (decimal, hex)
import           Text.XML.Expat.SAX
import           UnicodeVersion

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
    { _version = ver
    , _codePoint = cp
    , _age = x^?!ix "age"
    , _name = x^?!ix "na".name'
    , _name1 = x^?!ix "na1"
    , _block = x^?!ix "blk"
    , _generalCategory = x^?!ix "gc"
    , _combiningClass = x^?!ix "ccc"._Text.decimal
    , _bidiClass = x^?!ix "bc"
    , _bidiM = x^?!ix "Bidi_M".yn
    , _bidiMirroredGlyph = x^?ix "bmg".cp'
    , _bidiControl = x^?!ix "Bidi_C".yn
    , _bidiPairedBracketType = x^?!ix "bpt"
    , _bidiPairedBracket = x^?ix "bpb".cp'
    , _decompositionType = x^?!ix "dt"
    , _decomposition = x^..ix "dm".tworded.cp'
    , _compositionExclusion = x^?!ix "CE".yn
    , _compositionExclusionFull = x^?!ix "Comp_Ex".yn
    , _quickCheckNFC = x^?!ix "NFC_QC"
    , _quickCheckNFD = x^?!ix "NFD_QC"
    , _quickCheckNFKC = x^?!ix "NFKC_QC"
    , _quickCheckNFKD = x^?!ix "NFKD_QC"
    , _numericType = x^?!ix "nt"
    , _numericValue = x^?ix "nv".to (T.replace "/" "%")._Text._Show
    , _joiningType = x^?!ix "jt"
    , _joiningGroup = x^?!ix "jg"
    , _joinControl = x^?!ix "Join_C".yn
    , _linebreak = x^?!ix "lb"
    , _eastAsianWidth = x^?!ix "ea"
    , _uppercase = x^?!ix "Upper".yn
    , _lowercase = x^?!ix "Lower".yn
    , _simpleUppercaseMapping = x^?ix "suc".cp'
    , _simpleLowercaseMapping = x^?ix "slc".cp'
    , _simpleTitlecaseMapping = x^?ix "stc".cp'
    , _uppercaseMapping = x^..ix "uc".tworded.cp'
    , _lowercaseMapping = x^..ix "lc".tworded.cp'
    , _titlecaseMapping = x^..ix "tc".tworded.cp'
    , _caseIgnorable = x^?!ix "CI".yn
    , _cased = x^?!ix "Cased".yn
    , _script = x^?!ix "sc"
    , _scriptExtensions = x^..ix "scx".tworded
    , _hangulSyllableType = x^?!ix "hst"
    , _jamoShortName = x^?!ix "JSN"
    , _indicSyllabicCategory = x^?!ix "InSC"
    , _indicMatraCategory = x^?ix "InMC"
    , _indicPositionalCategory = x^?ix "InPC"
    , _idStart = x^?!ix "IDS".yn
    , _xidStart = x^?!ix "XIDS".yn
    , _idContinue = x^?!ix "IDC".yn
    , _xidContinue = x^?!ix "XIDC".yn
    , _patternSyntax = x^?!ix "Pat_Syn".yn
    , _patternWhiteSpace = x^?!ix "Pat_WS".yn
    , _dash = x^?!ix "Dash".yn
    , _quotationMark = x^?!ix "QMark".yn
    , _terminalPunctuation = x^?!ix "Term".yn
    , _sentenceTerminal = x^?!ix "STerm".yn
    , _diacritic = x^?!ix "Dia".yn
    , _extender = x^?!ix "Ext".yn
    , _softDotted = x^?!ix "SD".yn
    , _alphabetic = x^?!ix "Alpha".yn
    , _math = x^?!ix "Math".yn
    , _hexDigit = x^?!ix "Hex".yn
    , _asciiHexDigit = x^?!ix "AHex".yn
    , _defaultIgnorableCodePoint = x^?!ix "DI".yn
    , _logicalOrderException = x^?!ix "LOE".yn
    , _whiteSpace = x^?!ix "WSpace".yn
    , _graphemeBase = x^?!ix "Gr_Base".yn
    , _graphemeExtend = x^?!ix "Gr_Ext".yn
    , _graphemeClusterBreak = x^?!ix "GCB"
    , _wordBreak = x^?!ix "WB"
    , _sentenceBreak = x^?!ix "SB"
    , _ideographic = x^?!ix "Ideo".yn
    , _unifiedIdeographic = x^?!ix "UIdeo".yn
    , _idsBinaryOperator = x^?!ix "IDSB".yn
    , _idsTrinaryOperator = x^?!ix "IDST".yn
    , _radical = x^?!ix "Radical".yn
    , _deprecated = x^?!ix "Dep".yn
    , _variationSelector = x^?!ix "VS".yn
    , _noncharacter = x^?!ix "NChar".yn
    }
  return ()
  where name' = to (expandName cp)
        cp' :: Prism' Text Int
        cp' = expandCP (cp :: Int)
{-# INLINE insertChar #-}

loadUCD :: (Applicative m, MonadIO m) => UnicodeVersion -> Text -> ReaderT SqlBackend m ()
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
