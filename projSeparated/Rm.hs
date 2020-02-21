module Rm 
    ( rm
    , rmOne
    , rmDir
    ) where

import DataTypes
import Data.List(find, elemIndices)
import Cd
import LsAndPwd
import Control.Monad
import ExtraFuncs

rmOne :: String -> FSZipper -> Maybe FSZipper 
rmOne path dir@(Folder dirName dirChildren, dirPath)  = do
    if(find (=='/') path /= Nothing)
    then do
        let (toDeleteFileName, actualPath) = separateFileAndPath path
        (Folder name children, newPath) <- cd actualPath dir
        let newFolder = (Folder name $ filter (\z -> getName z /= toDeleteFileName) children, newPath)
        cd (pwd dir) newFolder 
    else do
        return (Folder dirName $ filter (\z -> getName z /= path) dirChildren, dirPath)
       
rmAll :: [String] -> FSZipper -> Maybe FSZipper 
rmAll args dir = 
    return dir >>= foldr (<=<) return (customReplicate args rmOne)     

rmFileOrFolder :: (String -> FSZipper -> Bool) -> [String] -> FSZipper -> Maybe FSZipper 
rmFileOrFolder ifFileOrFolderFunc args dir = 
    let  validationArray = map (\z -> ifFileOrFolderFunc z dir) args
         invalidIndices  = False `elemIndices` validationArray  --indices I want to remove
         actualArgs      = removeByIndices invalidIndices args
     in  rmAll actualArgs dir 

--ignores invalid directories because check if folder tells us if it is a valid folder
rmDir :: [String] -> FSZipper -> Maybe FSZipper
rmDir args dir = rmFileOrFolder checkIfFolder args dir 

--removes only files
rm :: [String] -> FSZipper -> Maybe FSZipper
rm args dir = rmFileOrFolder checkIfFile args dir
