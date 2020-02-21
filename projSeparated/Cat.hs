module Cat (cat) where 

import ExtraFuncs
import DataTypes
import Cd
import Rm
import LsAndPwd
import FileCreation
import Data.List(intersperse)

--used for cat > file 
writeData :: FSItem -> IO FSItem 
writeData (File name content) = do
   text <- getLine
   if(text == ".")
        then return (File name (tail content)) --tail removes the first "\n" added when the content was empty
   else 
        writeData (File name $ content ++ "\n" ++ text)

--goes to path and creates or replaces a file if it already exists
findFileToWrite :: String -> FSZipper -> IO (FSZipper)
findFileToWrite path fsZipp = do
    let (fileName, dirPath) = separateFileAndPath path
    let newZip = cd dirPath fsZipp
    if(newZip == Nothing)
        then return (fsZipp) 
    else do
        let (Just extractedZipp) = newZip
        if(isChild fileName extractedZipp) 
           then do 
               let (Just removedZipp) = rmOne fileName extractedZipp
               newFsItem <- writeData (File fileName "")
               let Just returnZipper = cd (pwd fsZipp) (addFSItem newFsItem removedZipp)
               return returnZipper
        else do 
            newFsItem <- writeData (File fileName "")
            let Just returnZipper = cd (pwd fsZipp) $ (addFSItem newFsItem extractedZipp)
            return returnZipper 
  
--checks if cat file1 file 2 ... file n > file n + 1 is properly entered
validateInput :: [String] -> Bool 
validateInput x = ((/= 0) . length . init $ x) && ((== ">") . last . init $ x)




catOne :: Bool -> String -> FSZipper -> Maybe String 
catOne _ [] _ = Nothing
catOne contentOf path fsZipp = do
     let (fileName, dirPath) = separateFileAndPath path
     newZip@(Folder _ children, _) <- cd dirPath fsZipp
     let fsItem = (filter (isName fileName) children)
     if(null fsItem || checkIfFolder path fsZipp) 
        then Nothing
     else if(contentOf) 
             then return ("Contents of file " ++ fileName ++ "\n" ++ (getData . head $ fsItem))
          else
             return $ getData . head $ fsItem
  
 --applies catOne for every argument with fsZipp and then it concatanates them. Adds newLines between file entries aswell. Args are reversed so they are displayed in the order they are entered
catMany :: Bool -> [String] -> FSZipper -> Maybe String
catMany contentOfMsg args fsZipp = foldr (mappend) (Just "") . intersperse (Just "\n") . map (\z -> z fsZipp) $ 
                                   (customReplicate (reverse args) (catOne contentOfMsg))

catIntoFile :: [String] -> FSZipper -> Maybe FSZipper
catIntoFile args fsZipp = do
   let saveFilePath = last args
   let (fileName, filePath) = separateFileAndPath saveFilePath
   text <- catMany False (init $ init args) fsZipp -- init init removes > and the save File
   newZipp <- cd filePath fsZipp
   if(checkIfFile saveFilePath fsZipp && isChild fileName newZipp) 
   then do
        removedZipp <- rmOne fileName newZipp 
        cd (pwd fsZipp) $ addFSItem (File fileName text) removedZipp 
   else cd (pwd fsZipp) $ addFSItem (File fileName text) newZipp 

catToStdOut :: [String] -> FSZipper -> IO FSZipper 
catToStdOut args fsZipp = do
    let (Just text) = catMany True args fsZipp
    mapM_ print $ lines text
    return fsZipp  


cat :: [String] -> FSZipper -> IO (Maybe FSZipper)
cat [] _ = return (Nothing)
cat args@(x:xs) fsZipp
  | (x == ">") && (length xs == 1) = do 
     newZipp <- findFileToWrite (concat xs) fsZipp
     return (Just newZipp)
  | (">" `elem` xs) && (validateInput xs) = return $ catIntoFile args fsZipp 
  | not (">" `elem` args) =  do
      catToStdOut args fsZipp
      return $ Just fsZipp 
  | otherwise = return Nothing
