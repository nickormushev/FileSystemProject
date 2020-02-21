module Main where

import Control.Monad
import Data.List (intersperse, find, elemIndices)
import Data.List.Split (splitOn)
import ExtraFuncs
import Cd
import Rm
import Cat
import FileCreation
import DataTypes
import LsAndPwd

--errors for commands that return a zipper
zipperErrorMessage :: FSZipper -> Maybe FSZipper -> String  -> IO FSZipper
zipperErrorMessage startItem item myError = do
    if(item == Nothing)
    then do 
        putStrLn myError
        return startItem 
    else do
        let (Just extractedItem) = item
        return extractedItem 

--errors for commands that return a string. They are different because theese nee to return a strin
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
      "rm"    -> zipperErrorMessage fsZipp (rm arguments fsZipp) "Non-existent file or directory"
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
   let myRoot = Folder "/" []
   let myFs = (myDisk, [])  
   loop myFs
   return ()
