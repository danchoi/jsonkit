{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module JSONKit where
import Data.Aeson
import Data.Attoparsec.Lazy as Atto hiding (Result)
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString.Lazy.Char8 as BL



foldObjects :: Value -> Value -> Value 
foldObjects (Object acc) (Object x) = Object $ x `HM.union` acc


decodeStream :: (FromJSON a) => BL.ByteString -> [a]
decodeStream bs = case decodeWith json bs of
    (Just x, xs) | xs == mempty -> [x]
    (Just x, xs) -> x:(decodeStream xs)
    (Nothing, _) -> []

decodeWith :: (FromJSON a) => Parser Value -> BL.ByteString -> (Maybe a, BL.ByteString)
decodeWith p s =
    case Atto.parse p s of
      Atto.Done r v -> f v r
      Atto.Fail _ _ _ -> (Nothing, mempty)
  where f v' r = (\x -> case x of 
                      Success a -> (Just a, r)
                      _ -> (Nothing, r)) $ fromJSON v'





