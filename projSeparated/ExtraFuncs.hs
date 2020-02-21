module ExtraFuncs
    ( splitPath
    , maybeBreak
    , removeByIndices
    , customReplicate
    , isName
    , isChild
    , separateFileAndPath
    ) where

import DataTypes
import Data.List(intersperse, find)

customReplicate :: [a] -> (a -> b -> c) -> [(b -> c)]
customReplicate [] _ = []
customReplicate (x:xs) f = customReplicate xs f ++ [(f x)]

separateFileAndPath :: String -> (String, String)
separateFileAndPath path = 
    (last split, concat . intersperse "/" . init $ split)
    where split = splitPath path 

isName :: String -> FSItem -> Bool 
isName name fsItem = name == getName fsItem

isChild :: String -> FSZipper -> Bool
isChild name (Folder _ children, _) = find (isName name) children /= Nothing 

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

splitPath :: String -> [String]
splitPath path = splitPathHelp path []

maybeBreak :: [a] -> (a -> Bool) -> [a] -> Maybe ([a], [a])
maybeBreak _ _ [] = Nothing
maybeBreak ls f rs@(x:xs) 
  | f x        = Just (ls, rs)
  | otherwise  = maybeBreak (ls ++ [x]) f xs 
