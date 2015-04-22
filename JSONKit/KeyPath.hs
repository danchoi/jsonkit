{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module JSONKit.KeyPath where
import Data.Aeson
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Attoparsec.Text as AT
import qualified Data.Vector as V
import Data.Scientific 
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersperse)
import Control.Applicative




data Config = Config {
    arrayDelim :: Text
  , nullValueString :: Text
  , trueString :: Text
  , falseString :: Text
  } deriving Show


-- | KeyPath may have an alias for the header output
data KeyPath = KeyPath [Key] (Maybe Text) deriving Show

data Key = Key Text | Index Int deriving (Eq, Show)

parseKeyPath :: Text -> [KeyPath]
parseKeyPath s = case AT.parseOnly pKeyPaths s of
    Left err -> error $ "Parse error " ++ err 
    Right res -> res

spaces = AT.many1 AT.space

pKeyPaths :: AT.Parser [KeyPath]
pKeyPaths = pKeyPath `AT.sepBy` spaces

pKeyPath :: AT.Parser KeyPath
pKeyPath = KeyPath 
    <$> (AT.sepBy1 pKeyOrIndex (AT.takeWhile1 $ AT.inClass ".["))
    <*> (pAlias <|> pure Nothing)

-- | A column header alias is designated by : followed by alphanum string after keypath
pAlias :: AT.Parser (Maybe Text)
pAlias = do
    AT.char ':'
    Just <$> AT.takeWhile1 (AT.inClass "a-zA-Z0-9_-")

pKeyOrIndex :: AT.Parser Key
pKeyOrIndex = pIndex <|> pKey

pKey = Key <$> AT.takeWhile1 (AT.notInClass " .[:")

pIndex = Index <$> AT.decimal <* AT.char ']'

evalToLineBuilder :: Config -> String -> [[Key]] -> Value -> B.Builder 
evalToLineBuilder config@Config{..} outDelim ks v = 
    mconcat $ intersperse (B.fromText . T.pack $ outDelim) $  map (flip (evalToBuilder config) v) ks

type ArrayDelimiter = Text

evalToList :: Config -> [[Key]] -> Value -> [Text]
evalToList c@Config{..} ks v = map (flip (evalToText c) v) ks

evalToBuilder :: Config -> [Key] -> Value -> B.Builder
evalToBuilder c k v = valToBuilder c $ evalKeyPath c k v

evalToText :: Config -> [Key] -> Value -> Text
evalToText c k v = valToText c $ evalKeyPath c k v

-- evaluates the a JS key path against a Value context to a leaf Value
evalKeyPath :: Config -> [Key] -> Value -> Value
evalKeyPath config [] x@(String _) = x
evalKeyPath config [] x@Null = x
evalKeyPath config [] x@(Number _) = x
evalKeyPath config [] x@(Bool _) = x
evalKeyPath config [] x@(Object _) = x
evalKeyPath config [] x@(Array v) | V.null v = Null
evalKeyPath config [] x@(Array v) = 
          let vs = V.toList v
              xs = intersperse (arrayDelim config) $ map (evalToText config []) vs
          in String . mconcat $ xs
evalKeyPath config (Key key:ks) (Object s) = 
    case (HM.lookup key s) of
        Just x          -> evalKeyPath config ks x
        Nothing -> Null
evalKeyPath config (Index idx:ks) (Array v) = 
      let e = (V.!?) v idx
      in case e of 
        Just e' -> evalKeyPath config ks e'
        Nothing -> Null
-- traverse array elements with additional keys
evalKeyPath _ ks@(Key key:_) (Array v) | V.null v = Null
evalKeyPath config@Config{..} ks@(Key key:_) (Array v) = 
      let vs = V.toList v
      in String . mconcat . intersperse arrayDelim $ map (evalToText config ks) vs
evalKeyPath _ ((Index _):_) _ = Null
evalKeyPath _ _ _ = Null

valToBuilder :: Config -> Value -> B.Builder
valToBuilder _ (String x) = B.fromText x
valToBuilder Config{..} Null = B.fromText nullValueString
valToBuilder Config{..} (Bool True) = B.fromText trueString
valToBuilder Config{..} (Bool False) = B.fromText falseString
valToBuilder _ (Number x) = 
    case floatingOrInteger x of
        Left float -> B.realFloat float
        Right int -> B.decimal int
valToBuilder _ (Object _) = B.fromText "[Object]"

valToText :: Config -> Value -> Text
valToText _ (String x) = x
valToText Config{..} Null = nullValueString
valToText Config{..} (Bool True) = trueString
valToText Config{..} (Bool False) = falseString
valToText _ (Number x) = 
    case floatingOrInteger x of
        Left float -> T.pack . show $ float
        Right int -> T.pack . show $ int
valToText _ (Object _) = "[Object]"


