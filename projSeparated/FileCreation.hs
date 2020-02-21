module FileCreation 
    ( touch
    , mkDir
    , addFSItem
    ) where 

import DataTypes
import ExtraFuncs
import Cd
import LsAndPwd

addFSItem :: FSItem -> FSZipper -> FSZipper
addFSItem fsItem (Folder name children, parents) = (Folder name $ fsItem : children, parents) 

createFileOrFolder :: String -> String -> FSZipper -> FSZipper
createFileOrFolder newFSItemName "file" fsZipp = addFSItem (File newFSItemName "") fsZipp
createFileOrFolder newFSItemName "folder" fsZipp = addFSItem (Folder newFSItemName []) fsZipp 

makeFileOrFolder :: String -> String -> FSZipper -> Maybe FSZipper 
makeFileOrFolder path fileOrFolder fsZipp = do
    let (newFSItemName, dirPath) = separateFileAndPath path
    dirZip <- cd dirPath fsZipp
    if(isChild newFSItemName dirZip)
        then Nothing
    else 
        cd (pwd fsZipp) $ createFileOrFolder newFSItemName fileOrFolder dirZip 

touch :: String -> FSZipper -> Maybe FSZipper
touch path fsZipp 
  | null path = Nothing
  | otherwise = makeFileOrFolder path "file" fsZipp

mkDir :: String -> FSZipper -> Maybe FSZipper
mkDir path fsZipp  
  | null path = Nothing
  | otherwise = makeFileOrFolder path "folder" fsZipp
