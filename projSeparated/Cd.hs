module Cd 
         ( cd
          , checkIfFile
          , checkIfFolder 
          ) where

import DataTypes 
import ExtraFuncs
import Control.Monad

openFolder :: Name -> FSZipper -> Maybe FSZipper 
openFolder folderName (Folder currentName children, path) = do 
    (ls, item:rs) <- maybeBreak [] (isFolderName folderName) children 
    return (item, Path currentName ls rs:path)

isFolderName :: String -> FSItem -> Bool
isFolderName name (Folder folderName items) = name == folderName
isFolderName _ (File _ _) = False 

goUp :: FSZipper -> Maybe FSZipper
goUp (_, []) = Nothing
goUp (item, (Path name ls rs : xs)) = Just (Folder name (ls ++ [item] ++ rs), xs) 

goToRoot :: FSZipper -> Maybe FSZipper
goToRoot z@(Folder name items, path) = 
    return z >>= foldr (<=<) return (replicate (length path) goUp)  

move :: String -> FSZipper -> Maybe FSZipper
move fileName dir
  | fileName == ""   = goToRoot dir 
  | fileName == ".." = goUp dir
  | otherwise        = openFolder fileName dir  

cd :: String -> FSZipper -> Maybe FSZipper
cd path dir = do
    let split = splitPath path
    return dir >>= foldr (<=<) return (customReplicate split move)         

checkIfFile :: String -> FSZipper -> Bool
checkIfFile path fsZipp = 
    let nothing = cd path fsZipp --if it is a file cd should return Nothing
    in nothing == Nothing  

checkIfFolder :: String -> FSZipper -> Bool
checkIfFolder path fsZipp = not $ checkIfFile path fsZipp
