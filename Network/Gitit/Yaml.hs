module Network.Gitit.Yaml where

import Data.Maybe
import Data.Object
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS

string :: (IsYamlScalar s) => s -> YamlObject
string str = Scalar (toYamlScalar str)

strings :: [String] -> YamlObject
strings ss =
  Sequence $ map (Scalar . toYamlScalar) ss

getSubKey :: (IsYamlScalar a) => BS.ByteString -> BS.ByteString -> YamlObject -> Maybe a
getSubKey key subkey obj = do
  attr <- getAttr key obj
  r <- getAttr subkey attr
  getScalar r

getSubObj :: BS.ByteString -> BS.ByteString -> YamlObject -> Maybe YamlObject
getSubObj key subkey obj = do
  attr <- getAttr key obj
  getAttr subkey attr

getAttr :: BS.ByteString -> YamlObject -> Maybe YamlObject
getAttr key (Mapping pairs) = lookup (toYamlScalar key) pairs
getAttr key (Sequence lst) =
  case catMaybes $ map (getAttr key) lst of
    [x] -> Just x
    _   -> Nothing
getAttr _ (Scalar _) = Nothing

getName :: YamlObject -> String
getName (Mapping ((n,_):_)) = fromYamlScalar n
getName _                   = "user"

getString :: YamlObject -> String
getString (Scalar x) = fromYamlScalar x
getString _          = ""

getScalar :: (IsYamlScalar a) => YamlObject -> Maybe a 
getScalar (Scalar x) = Just (fromYamlScalar x)
getScalar _          = Nothing

getList :: YamlObject -> [String]
getList (Sequence lst) = map getString lst
getList _              = []

getObjList :: YamlObject -> [YamlObject]
getObjList (Sequence lst) = lst
getObjList _              = []

getListAttr :: (IsYamlScalar s) => BS.ByteString -> YamlObject -> [s]
getListAttr key y = 
  case getAttr key y of
    Nothing -> []
    Just (Scalar x) -> [fromYamlScalar x]
    Just (Sequence lst) -> catMaybes $ map getScalar lst
    Just _              -> []

