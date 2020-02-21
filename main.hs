import Control.Monad
import Data.List (intersperse, find, elemIndices)
import Data.List.Split (splitOn)

type Name = String
type Data = String
data FSItem = File { getName :: Name, getData :: Data } | Folder { getName :: Name, getChildren :: [FSItem] } deriving (Eq) 
data FSPath = Path Name [FSItem] [FSItem] deriving (Eq) --Used to keep track of which directory I am in. Basically It is a parent their names and the files that were before and after the one you are in in its children
type FSZipper = (FSItem, [FSPath]) --A combination between the current directory and it's parents 

instance Show FSItem where 
    show (Folder name children) = name 
    show (File name d) = name 

instance Show FSPath where 
    show (Path name ls rs) = name

--variable used for testing
myDisk :: FSItem  
myDisk = 
    Folder "/"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source_code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]

checkIfFile :: String -> FSZipper -> Bool
checkIfFile path fsZipp = 
    let nothing = cd path fsZipp --if it is a file cd should return Nothing
    in nothing == Nothing  

checkIfFolder :: String -> FSZipper -> Bool
checkIfFolder path fsZipp = not $ checkIfFile path fsZipp

removeByIndices :: [Int] -> [a] -> [a]
removeByIndices [] y = y 
removeByIndices (x:xs) y = 
    let (left, _:right) = splitAt x y
     in removeByIndices (map (\z -> z - 1) xs) (left ++ right)

splitPathHelp :: String -> [String] -> [String]
splitPathHelp [] res = res
splitPathHelp path splitRes
  | restOfPath == []    = splitPathHelp [] (splitRes ++ [pathSegment])
  | pathSegment == "."  = splitPathHelp (tail restOfPath) splitRes  
  | otherwise           = splitPathHelp (tail restOfPath) (splitRes ++ [pathSegment])
  where (pathSegment, restOfPath) = break (=='/') path

--splits path like foo/bar into a list of [foo, bar]. It aslo remove . like ./foo/./bar -> [foo, bar]. If we start with a / the function adds a "" for an element. Example -> /foo/bar -> ["", foo, bar]
splitPath :: String -> [String]
splitPath path = splitPathHelp path []

--used for openFolder so I can return Nothing if a folder is not in the children
maybeBreak :: [a] -> (a -> Bool) -> [a] -> Maybe ([a], [a])
maybeBreak _ _ [] = Nothing
maybeBreak ls f rs@(x:xs) 
  | f x        = Just (ls, rs)
  | otherwise  = maybeBreak (ls ++ [x]) f xs 

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

--I used this so I can easily create the same function n times buth with a diffrenet first argument. 
--Used for cd and others so I can support composiotion of monads
customReplicate :: [a] -> (a -> b -> c) -> [(b -> c)]
customReplicate [] _ = []
customReplicate (x:xs) f = customReplicate xs f ++ [(f x)]

--cd's into the first directory and then gives the result Maybe FSZipper(Monad) to the second cd command and so on and so forth
cd :: String -> FSZipper -> Maybe FSZipper
cd path dir = do
    let split = splitPath path
    return dir >>= foldr (<=<) return (customReplicate split move)

--enters a directory and displays its contents
--Here I used the redifined show command
ls :: String -> FSZipper -> Maybe String 
ls "" z@(Folder name children, prevPath) =  Just $ concat . intersperse " " . map show $ children
ls path z@(Folder name children, prevPath) = do
    (Folder dirName dirChildren, dirPath) <- (cd path z)
    Just $ concat . intersperse " " . map show $ dirChildren

--tail is for removing the interspersed / between / and the next item from the path
--tha path needs to be reversed because it has the directory we are in first and the root last
--intersperse adds the / and concat flattens the list of elements
--show turns them into strings and i have redifined it for FSPath 
pwd :: FSZipper -> String
pwd (_, []) = "/"
pwd (Folder name _, path) = (tail . concat . intersperse "/" . map show . reverse $ path) ++ "/" ++ name

--the commented part is because I had an issue at one point but it seems to work fine without it now. I can't remember the issue
rmOne :: String -> FSZipper -> Maybe FSZipper 
rmOne path dir@(Folder dirName dirChildren, dirPath)  = do
    --if(find (=='/') path /= Nothing)
    --then do
        let (toDeleteFileName, actualPath) = separateFileAndPath path
        (Folder name children, newPath) <- cd actualPath dir
        let newFolder = (Folder name $ filter (\z -> getName z /= toDeleteFileName) children, newPath)
        cd (pwd dir) newFolder 
    --else do
    --return (Folder dirName $ filter (\z -> getName z /= path) dirChildren, dirPath)

--attempts to removes all args. If one is invalid none are removed
rmAll :: [String] -> FSZipper -> Maybe FSZipper 
rmAll args dir = 
    return dir >>= foldr (<=<) return (customReplicate args rmOne)     

--filters the arguments based on a functio that says if they are files or folders. Used by rmdir and rm
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

--used the same peace of code really often so I put it in a function to save space
--It separates the directory from the fileName Example: /test/rest/main.hs -> (main.hs, /tes/rest)
separateFileAndPath :: String -> (String, String)
separateFileAndPath path = 
    (last split, concat . intersperse "/" . init $ split) --Cd function uses a string so i turned the arguments without the last into a string  
    where split = splitPath path

--used by createFileOrFolder and some other functions to add files or folders to the children
addFSItem :: FSItem -> FSZipper -> FSZipper
addFSItem fsItem (Folder name children, parents) = (Folder name $ fsItem : children, parents) 

createFileOrFolder :: String -> String -> FSZipper -> FSZipper
createFileOrFolder newFSItemName "file" fsZipp = addFSItem (File newFSItemName "") fsZipp
createFileOrFolder newFSItemName "folder" fsZipp = addFSItem (Folder newFSItemName []) fsZipp 

isName :: String -> FSItem -> Bool 
isName name fsItem = name == getName fsItem

isChild :: String -> FSZipper -> Bool
isChild name (Folder _ children, _) = find (isName name) children /= Nothing 

--first I check if the file whith the same name as the one I am trying to create is already there. If it is I return Nothing
--then i send it to the createFileOrFolder which actually creates the file and i use cd on the returned fsZipper to go back to the dir
--from where the command is called
makeFileOrFolder :: String -> String -> FSZipper -> Maybe FSZipper 
makeFileOrFolder path fileOrFolder fsZipp = do
    let (newFSItemName, dirPath) = separateFileAndPath path
    dirZip <- cd dirPath fsZipp
    if(isChild newFSItemName dirZip)
        then Nothing
    else 
        cd (pwd fsZipp) $ createFileOrFolder newFSItemName fileOrFolder dirZip 

--creates File
touch :: String -> FSZipper -> Maybe FSZipper
touch path fsZipp 
  | null path = Nothing
  | otherwise = makeFileOrFolder path "file" fsZipp

--creates Dir
mkDir :: String -> FSZipper -> Maybe FSZipper
mkDir path fsZipp  
  | null path = Nothing
  | otherwise = makeFileOrFolder path "folder" fsZipp

--used for cat > file
--writes data untill only . is entered 
writeData :: FSItem -> IO FSItem 
writeData (File name content) = do
   text <- getLine
   if(text == ".")
        then return (File name (tail content)) --tail removes the first "\n" added when the content was empty
   else 
        writeData (File name $ content ++ "\n" ++ text)

--used for cat > file to check if the file exists and replace it if it does or create a new one if it doesn't
--Also checks if the folder of the file we are trying to create is valid
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
  
--returns a string of the contents of a file
--if contentOf is True it adds contents of file to the string which I use for when I cat many files to std::out so I see the different files
--It is False when I cat files to a file, because I don't care from where the data comes from I want to see it. Idk it seemed better this way
catOne :: Bool -> String -> FSZipper -> Maybe String 
catOne _ [] _ = Nothing
catOne contentOf path fsZipp = do
    let (fileName, dirPath) = separateFileAndPath path
    newZip@(Folder _ children, _) <- cd dirPath fsZipp
    let fsItem = (filter (isName fileName) children)
    if(null fsItem || checkIfFolder path fsZipp) 
       then Nothing
    else if(contentOf) 
            then return ("Contents of file " ++ fileName ++ ":\n" ++ (getData . head $ fsItem))
         else
            return $ getData . head $ fsItem

 --applies catOne for every argument with fsZipp and then it concatanates them. Adds newLines between file entries aswell. Args are reversed so they are displayed in the order they are entered
catMany :: Bool -> [String] -> FSZipper -> Maybe String
catMany contentOfMsg args fsZipp = foldr (mappend) (Just "") . intersperse (Just "\n") . map (\z -> z fsZipp) $ 
                                   (customReplicate (reverse args) (catOne contentOfMsg))

--cat <file 1> ... <file n> > <file n+1>
--does validation if a file exists and replaces it
--Might have to remove checkIfFile from the if because I might create duplicate files and folders which the rest of the project does not support
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

--prints all file data to the terminal
catToStdOut :: [String] -> FSZipper -> IO FSZipper 
catToStdOut args fsZipp = do
    let (Just text) = catMany True args fsZipp
    mapM_ print $ lines text
    return fsZipp  

--checks if cat file1 file 2 ... file n > file n + 1 is properly entered
validateInput :: [String] -> Bool 
validateInput x = ((/= 0) . length . init $ x) && ((== ">") . last . init $ x)

--used to select which cat to use
--first I check for cat > file name. cat is already read and i support just one file to write so x must be > and length must be 1
--next i check for cat file1 ... filen > file n + 1. The element before last must be > and one file must exist before > and after(validateInput)
--last is just the normal cat file1 ... file n  and anything else is considered invalid
--cat > file
--cat file
--cat .... > file
cat :: [String] -> FSZipper -> IO (Maybe FSZipper)
cat [] _ = return (Nothing)
cat args@(x:xs) fsZipp
  | (x == ">") && (length xs == 1) = do 
     newZipp <- findFileToWrite (concat xs) fsZipp
     return (Just newZipp)
  | validateInput args = return $ catIntoFile args fsZipp 
  | not (">" `elem` args) =  do
      catToStdOut args fsZipp
      return $ Just fsZipp 
  | otherwise = return Nothing

--errors for commands that return a zipper
--if Nothing is returned an error is shown
zipperErrorMessage :: FSZipper -> Maybe FSZipper -> String  -> IO FSZipper
zipperErrorMessage startItem item myError = do
    if(item == Nothing)
    then do 
        putStrLn myError
        return startItem 
    else do
        let (Just extractedItem) = item
        return extractedItem 

--errors for commands that return a string. They are different because theese need to return the startFSZipper and not a new one 
stringErrorMessage :: FSZipper -> Maybe String -> String -> IO FSZipper 
stringErrorMessage startZipper msg myError = do 
    if(msg == Nothing)
    then do
        putStrLn myError
        return startZipper
    else do
        let (Just extractedMsg) = msg 
        putStrLn extractedMsg 
        return startZipper 

commandReader :: (String, FSZipper) -> IO FSZipper
commandReader (cmd, fsZipp) = 
    case command of 
      "cd"    -> zipperErrorMessage fsZipp (cd (concat arguments) fsZipp) "Invalid directory!"
      "ls"    -> stringErrorMessage fsZipp (ls (concat arguments) fsZipp) "Invalid file or directory!"
      "pwd"   -> stringErrorMessage fsZipp (Just (pwd fsZipp)) "This should not be able to fail. Huge problem if it does" 
      "rm"    -> zipperErrorMessage fsZipp (rm arguments fsZipp) "Non-existent file"
      "touch" -> zipperErrorMessage fsZipp (touch (concat arguments) fsZipp) "Duplicate file or wrong path"
      "mkdir" -> zipperErrorMessage fsZipp (mkDir (concat arguments) fsZipp) "Duplicate folder or wrong path"
      "rmdir" -> zipperErrorMessage fsZipp (rmDir arguments fsZipp) "Non-existent directory"
      "cat"   -> do
          maybeFsZipp <- cat arguments fsZipp 
          zipperErrorMessage fsZipp maybeFsZipp "Non existant directory or maybe too many arguments after >. Fix your command"
      _       -> return fsZipp  
    where (command:args) = splitOn " " cmd 
          arguments = filter (/="") args --if I write a ton of spaces after the command I get these invalid arguments "". Example: [validArg, ""]. So I remove them 

loop :: FSZipper -> IO ()
loop fsZipp = do
    putStr $ (pwd fsZipp) ++ " $/> " 
    cmd <- getLine 
    if(cmd == "quit" || cmd == "exit")
    then
      return ()
    else do 
       newZip <- commandReader (cmd, fsZipp)
       loop newZip 


main :: IO ()
main = do
   --let myRoot = Folder "/" []
   let myFs = (myDisk, [])  
   loop myFs
   return ()
