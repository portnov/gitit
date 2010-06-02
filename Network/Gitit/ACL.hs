{-# LANGUAGE TypeSynonymInstances, ViewPatterns #-}

module Network.Gitit.ACL where

import Data.List
import Data.Object
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((</>))

import Network.Gitit.Yaml

type Objects = [GititObject]
data GititObject = GPage String | GDirectory String | AllPages
  deriving (Eq)

instance Show GititObject where
  show (GPage s) = s
  show (GDirectory s) = s ++ "/"
  show AllPages = "all"

instance IsYamlScalar GititObject where
  toYamlScalar (GPage s) = toYamlScalar s
  toYamlScalar (GDirectory s) = toYamlScalar (s++"/")
  toYamlScalar AllPages = toYamlScalar "all"

  fromYamlScalar s@(last . fromYamlScalar -> '/') = GDirectory (init $ fromYamlScalar s)
  fromYamlScalar (fromYamlScalar -> "all") = AllPages
  fromYamlScalar s = GPage (fromYamlScalar s)

(<#>) :: GititObject -> GititObject -> GititObject 
(GPage p) <#> _ = GPage p
(GDirectory d1) <#> (GDirectory d2) = GDirectory (d1 </> d2)
(GDirectory d) <#> (GPage p) = GPage (d </> p)
(GDirectory d) <#> AllPages = GDirectory d
AllPages <#> x = x

type Subjects = [Subject]
data Subject = UserS String | GroupS String | AllUsers
  deriving (Eq)

instance Show Subject where
  show (UserS u) = u
  show (GroupS g) = g ++ "@"
  show AllUsers = "all"

instance IsYamlScalar Subject where
  toYamlScalar (UserS u) = toYamlScalar u
  toYamlScalar (GroupS g) = toYamlScalar (g ++ "@")
  toYamlScalar AllUsers = toYamlScalar "all"

  fromYamlScalar s@(last . fromYamlScalar -> '@') = GroupS (init $ fromYamlScalar s)
  fromYamlScalar (fromYamlScalar -> "all") = AllUsers 
  fromYamlScalar s = UserS (fromYamlScalar s)
  
data PermitSpec = 
  PermitSpec PermitOp Permissions
  deriving (Eq)

instance Show PermitSpec where
  show (PermitSpec op ps) = show op ++ ": " ++ show ps

data Permission = 
    ReadP
  | WriteP
  | CreateP
  | DeleteP
  deriving (Eq,Ord)

newtype Permissions = Permissions {permissionsList :: [Permission]}
  deriving (Eq)

instance Show Permission where
  show ReadP = "read"
  show WriteP = "write"
  show CreateP = "create"
  show DeleteP = "delete"

allPermited :: Permissions -> Bool
allPermited (sort . permissionsList -> [ReadP, WriteP, CreateP, DeleteP]) = True
allPermited _ = False

instance Show Permissions where
  show p | allPermited p = "all"
  show lst = intercalate ", " $ map show (permissionsList lst)

data PermitOp = SetPermissions | UnsetPermissions
  deriving (Eq)

instance Show PermitOp where
  show SetPermissions = "permit"
  show UnsetPermissions = "deny"

data Rule =
    SimpleRule Objects Subjects PermitSpec
  | Rule Objects Subjects [Rule]
  deriving (Eq,Show)

data LinearRule = 
  LR {
    objects :: Objects,
    subjects :: Subjects,
    permitOp :: PermitOp,
    permissions :: Permissions }
  deriving (Eq)

type ACL = [LinearRule]

instance Show LinearRule where
  show (LR os ss po ps) = show os ++ ", " ++ show ss ++ ": " ++ show po ++ " " ++ show ps

rulesToYaml :: [Rule] -> YamlObject
rulesToYaml rules = Sequence (map ruleToYaml rules)

gToY :: GititObject -> YamlObject
gToY (GPage s) = string s
gToY (GDirectory s) = string (s ++ "/")
gToY AllPages = string "all"

objsList :: [GititObject] -> YamlObject
objsList [] = Sequence []
objsList [x] = gToY x
objsList lst = Sequence $ map gToY lst

subjToY :: Subject -> YamlObject
subjToY (UserS s) = string s
subjToY (GroupS s) = string (s ++ "@")
subjToY AllUsers = string "all"

subjsList :: [Subject] -> YamlObject
subjsList [] = Sequence []
subjsList [x] = subjToY x
subjsList lst = Sequence $ map subjToY lst

permsList :: Permissions -> YamlObject
permsList (Permissions [p]) = string (show p)
permsList p | allPermited p = string "all"
permsList (Permissions ps) = Sequence $ map (string . show) ps

ruleToYaml :: Rule -> YamlObject
ruleToYaml (SimpleRule objs subjs spec) = Mapping (os ++ ss ++ sps spec)
  where
    os | null objs = []
       | otherwise = [(toYamlScalar "objects", objsList objs)]

    ss | null subjs = []
       | otherwise  = [(toYamlScalar "subjects", subjsList subjs)]

    sps (PermitSpec SetPermissions ps) = [(toYamlScalar "permit", permsList ps)]
    sps (PermitSpec UnsetPermissions ps) = [(toYamlScalar "deny", permsList ps)]
ruleToYaml (Rule objs [] rules) = Mapping [(toYamlScalar "objects", objsList objs),
                                           (toYamlScalar "rules",   rulesToYaml rules)]
ruleToYaml (Rule [] subjs rules) = Mapping [(toYamlScalar "subjects", subjsList subjs),
                                            (toYamlScalar "rules",    rulesToYaml rules)]
ruleToYaml (Rule objs subjs rules) = Mapping [(toYamlScalar "objects",  objsList objs),
                                              (toYamlScalar "subjects", subjsList subjs),
                                              (toYamlScalar "rules",    rulesToYaml rules)]

yamlToRules :: YamlObject -> [Rule]
yamlToRules (Sequence ys) = map yamlToRule ys
yamlToRules _ = []

yamlToRule :: YamlObject -> Rule
yamlToRule y =
    case getName y of
      "objects" -> 
        case getAttr (BS.pack "rules") y of
          Nothing -> SimpleRule (getObjects y) (getSubjects y) (getPermissions y)
          Just rs -> Rule (getObjects y) [] (yamlToRules rs)
      "subjects" ->
        case getAttr (BS.pack "rules") y of
          Nothing -> SimpleRule (getObjects y) (getSubjects y) (getPermissions y)
          Just rs -> Rule [] (getSubjects y) (yamlToRules rs)

getObjects :: YamlObject -> Objects
getObjects y = map fromYamlScalar $ getListAttr (BS.pack "objects") y

getSubjects :: YamlObject -> Subjects 
getSubjects y = map fromYamlScalar $ getListAttr (BS.pack "subjects") y

getPermissions :: YamlObject -> PermitSpec
getPermissions y =
    case getListAttr (BS.pack "permit") y of
      ["all"] -> PermitSpec SetPermissions (Permissions [ReadP, WriteP, CreateP, DeleteP])
      [] ->
        case getListAttr (BS.pack "deny") y of
          ["all"] -> PermitSpec UnsetPermissions (Permissions [ReadP, WriteP, CreateP, DeleteP])
          lst     -> PermitSpec UnsetPermissions (Permissions $ map readPermission lst)
      lst     -> PermitSpec SetPermissions (Permissions $ map readPermission lst)
  where
    readPermission "read" = ReadP
    readPermission "write" = WriteP
    readPermission "create" = CreateP
    readPermission "delete" = DeleteP
    readPermission x = error $ "Unknown permission: " ++ x

linearize :: [Rule] -> ACL
linearize = concatMap linearize'

linearize' :: Rule -> ACL
linearize' (SimpleRule os ss (PermitSpec po ps)) = [LR os ss po ps]
linearize' (Rule os ss rules) = (LR os ss SetPermissions (Permissions [])) `composeACL` rules

composeACL :: LinearRule -> [Rule] -> ACL
composeACL lr ps = concatMap (compose' lr) ps
  where
    compose' :: LinearRule -> Rule -> [LinearRule]
    compose' (LR los lss _ lps) (SimpleRule sos sss (PermitSpec spo sps)) =
        [LR (composeObjects los sos) (lss ++ sss) spo (composePermissions lps sps)]
    compose' (LR los lss _ lps) (Rule os ss rules) = 
        (LR (composeObjects los os) (lss ++ ss) SetPermissions lps) `composeACL` rules

    composeObjects os [] = os
    composeObjects [] os = os
    composeObjects os1 os2 = 
        [o1 <#> o2 | o1 <- os1, o2 <- os2]

    composePermissions (Permissions p1) (Permissions p2) = Permissions (nub $ sort $ p1 ++ p2)
