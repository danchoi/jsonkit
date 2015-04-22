{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Main where
import JSONKit
import qualified Options.Applicative as O
import Data.Aeson
import Data.Attoparsec.Lazy as Atto hiding (Result)
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString.Lazy.Char8 as BL

data Options = Options

main = do
    input <- BL.getContents
    let xs :: [Value]
        xs = decodeStream input
        -- fold successive objects into the first
        result = case xs of 
          [xs] -> xs
          [] -> Null
          x:xs -> foldl foldObjects x xs 
    BL.putStrLn . encode $ result


