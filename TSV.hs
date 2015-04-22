{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module Main where
import JSONKit.Base
import JSONKit.KeyPath
import Data.Aeson
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.List (intersperse)
import qualified Data.List 
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Monad (when)
import Data.ByteString.Lazy as BL hiding (map, intersperse)
import Data.Attoparsec.ByteString.Char8 (endOfLine, sepBy)
import qualified Data.Attoparsec.Text as AT
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Scientific 
import System.Environment (getArgs)
import qualified Options.Applicative as O
import qualified Text.CSV as CSV
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B

data Options = Options { 
    jsonExpr :: String
  , optArrayDelim :: Text
  , outputMode :: OutputMode
  , showHeader :: Bool
  , nullString :: Text
  , optTrueString :: Text
  , optFalseString :: Text
  , debugKeyPaths :: Bool
  } deriving Show

data OutputMode = TSVOutput { delimiter :: String } | CSVOutput deriving (Show)

parseOpts :: O.Parser Options
parseOpts = Options 
  <$> O.argument O.str (O.metavar "FIELDS")
  <*> (T.pack 
        <$> O.strOption (O.metavar "DELIM" <> O.value "," <> O.short 'a' <> O.help "Concatentated array elem delimiter. Defaults to comma."))
  <*> (parseCSVMode <|> parseTSVMode)
  <*> O.flag False True (O.short 'H' <> O.help "Include headers")
  <*> (T.pack 
        <$> O.strOption (O.value "null" 
            <> O.short 'n' <> O.long "null-string"
            <> O.metavar "STRING" <> O.help "String to represent null value. Default: 'null'"))
  <*> (T.pack 
        <$> O.strOption (O.value "t" 
            <> O.short 't' <> O.long "true-string"
            <> O.metavar "STRING" <> O.help "String to represent boolean true. Default: 't'"))
  <*> (T.pack 
        <$> O.strOption (O.value "f" 
            <> O.short 'f' <> O.long "false-string"
            <> O.metavar "STRING" <> O.help "String to represent boolean false. Default: 'f'"))
  <*> O.switch (O.long "debug" <> O.help "Debug keypaths")

parseCSVMode = O.flag' CSVOutput (O.short 'c' <> O.long "csv" <> O.help "Output CSV")

parseTSVMode = TSVOutput 
    <$> (O.strOption 
          (O.metavar "DELIM" <> O.value "\t" <> O.short 'd' <> O.help "Output field delimiter. Defaults to tab."))

opts = O.info (O.helper <*> parseOpts)
          (O.fullDesc 
            <> O.progDesc "Transform JSON objects to TSV. \
                    \On STDIN provide an input stream of whitespace-separated JSON objects."
            <> O.header "jsontsv"
            <> O.footer "See https://github.com/danchoi/jsontsv for more information.")

main = do
  Options{..} <- O.execParser opts
  x <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream x
      ks = parseKeyPath $ T.pack jsonExpr
      -- keypaths without alias info:
      ks' = [ks' | KeyPath ks' _ <- ks]
  when debugKeyPaths $
     Prelude.putStrLn $ "key Paths " ++ show ks
  when showHeader $ do
    let hs = [case alias of 
                Just alias' -> T.unpack alias'
                Nothing -> jsonExpr  
              | (KeyPath _ alias, jsonExpr) <- Data.List.zip ks (words jsonExpr)] 
              -- Note `words` introduces a potential bug is quoted aliases are allowed
              -- See https://github.com/danchoi/jsonxlsx/commit/9aedb4bf97cfa8d5635edc4780bfbf9b79b6f2ec
    case outputMode of 
      TSVOutput delim -> Prelude.putStrLn . Data.List.intercalate delim $ hs
      CSVOutput -> Prelude.putStrLn . CSV.printCSV $ [hs]
  let config = Config {   
                  arrayDelim = optArrayDelim
                , nullValueString = nullString
                , trueString = optTrueString
                , falseString = optFalseString}
  case outputMode of 
    TSVOutput delim -> mapM_ (TL.putStrLn . B.toLazyText . evalToLineBuilder config delim ks') xs
    CSVOutput -> Prelude.putStrLn . CSV.printCSV $ map (map T.unpack . evalToList config ks') $  xs

