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


solution1 inp = length $ countParents parsed [] "shinygold"
  where parsed = parseRules inp

solution2 =  (flip countChildren $ "shinygold") . parseRules

parseRules = map ( mapPair join (
             map ( mapPair (read :: String -> Int) id . splitAt 1) 
                 . filter (/= "noother") 
                 . splitWhen (== ',') 
                 . join 
                 . tail ) 
           . break (== "contain") 
           . filter (/= "") . map mapWords
           . words)
  where 
    mapPair f g (a, b) = (f a, g b) 
    mapWords word 
      | word `elem` excludeWords = ""
      | word `elem` replaceWords = ","
      | otherwise = word
    excludeWords = ["bag", "bags", "bags.", "bag."]
    replaceWords = ["bag,", "bags,"]


countParents :: [(String, [(Int, String)])] -> [String] -> String  -> [String]
countParents rules visited bag = 
  directLeaves ++ nub (concatMap (countParents rules (directLeaves ++ visited)) directLeaves)
  where directLeaves = map fst 
                     $ filter (elem bag . map snd .  snd) 
                     $ filter (not . (flip elem $ visited) . fst) rules

countChildren :: [(String, [(Int, String)])] -> String  -> Int
countChildren rules bag = children
  where children = case find ((== bag) . fst) rules of
                     Nothing -> 0
                     Just (_, []) -> 0
                     Just (_, xs) -> foldl' (\acc (coef, bag') -> 
                       acc + coef + (coef * (countChildren rules bag'))) 0 xs


splitWhen :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitWhen f l = foldr (\x acc -> if f x then [] : acc else (x : head acc) : tail acc) [[]] l

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count pr = foldl' (\acc a -> if pr a then acc + 1 else acc) 0

testInp = [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
          , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
          , "bright white bags contain 1 shiny gold bag."
          , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
          , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
          , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
          , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
          , "faded blue bags contain no other bags."
          , "dotted black bags contain no other bags."]
