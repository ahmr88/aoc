{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List
import Data.Char
import Control.Monad

main = do
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singleLines = lines contents
        
        -- print $ solution1 singleLines
        print $ solution2 singleLines
        -- print $ length singleLines

        hClose handle


solution1 = sum . map length . map (removeDups . join) . parseGroups

solution2 inp = sum $ map length $ zipWith zipF parsed $ map (removeDups . join) parsed
  where parsed = parseGroups inp
        zipF :: [String] -> String -> String
        zipF gp qs = filter (\c -> all (elem c) gp) qs

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count pr = foldl' (\acc a -> if pr a then acc + 1 else acc) 0

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = if elem x xs then newTail else x : newTail
  where newTail = removeDups xs

parseGroups = map (concatMap words) 
            . filter (/= [""]) 
            . groupBy (\a b -> a /= "" && b /= "")
  where
    tailSnd (a, b) = (a, tail b)

testInput = [ "abc"
            , ""
            , "a"
            , "b"
            , "c"
            , ""
            , "ab"
            , "ac"
            , ""
            , "a"
            , "a"
            , "a"
            , "a"
            , ""
            , "b"]

