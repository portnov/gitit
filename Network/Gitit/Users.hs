{-# LANGUAGE OverloadedStrings #-}

module Network.Gitit.Users where

import Data.Maybe
import Data.Object
import Data.Object.Yaml
-- import Data.Default
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

import Network.Gitit.Types
import Network.Gitit.Yaml

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

userToYaml :: User -> YamlObject
userToYaml (User name (Password salt pass) mail groups) = 
  Mapping [
    (toYamlScalar name, Mapping [
      (mkScalar "password", string pass),
      (mkScalar "salt",     string salt),
      (mkScalar "email",    string mail),
      (mkScalar "groups",   strings groups)])]
  where
    mkScalar :: String -> YamlScalar
    mkScalar = toYamlScalar
