module LsAndPwd 
    ( pwd
    , ls
    ) where

import DataTypes
import Cd
import Data.List (intersperse)

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


