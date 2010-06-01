{-# LANGUAGE OverloadedStrings #-}

module Network.Gitit.Users where

import Data.Maybe
import Data.Object
import Data.Object.Yaml
-- import Data.Default
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

import Network.Gitit.Types
-- import Network.Gitit.Util (readFileUTF8)

string :: (IsYamlScalar s) => s -> YamlObject
string str = Scalar (toYamlScalar str)

pair :: (IsYamlScalar s) => (BS.ByteString, s) -> (YamlScalar, YamlObject)
pair (name,value) = (toYamlScalar name, string value)

userToYaml :: User -> YamlObject
userToYaml user =
  Mapping [(toYamlScalar (uUsername user),
    Mapping ((map pair [
      ("password", pHashed (uPassword user)),
      ("salt",     pSalt   (uPassword user)),
      ("email",    uEmail user)]) ++
      [(toYamlScalar ("groups" :: String), strings (uGroups user))]))]

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
getAttr key (Scalar sc) = Nothing

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

yamlToUser :: YamlObject -> User
yamlToUser y = User name (Password salt pass) mail groups
  where
    name = getName y
    name' = BS.pack name
    salt = fromMaybe "" $ getSubKey name' "salt" y
    pass = fromMaybe "" $ getSubKey name' "password" y
    mail = fromMaybe "" $ getSubKey name' "email" y
    groups = case getSubObj name' "groups" y of
               Nothing -> []
               Just x -> getList x

strings :: [String] -> YamlObject
strings ss =
  Sequence $ map (Scalar . toYamlScalar) ss

loadUsers :: FilePath -> IO (M.Map String User)
loadUsers path = do
    list <- loadUsers' 
    return $ M.fromList [(uUsername user, user) | user <- list]
  where
    loadUsers' = do
      str <- BS.readFile path
      lst <- decode str
      let users = getObjList lst
      return $ map yamlToUser users

saveUsers :: FilePath -> (M.Map String User) -> IO ()
saveUsers path users = do
  let users' = map snd $ M.assocs users
      ys = map userToYaml users'
      y = Sequence ys
      str = encode y
  BS.writeFile path str

