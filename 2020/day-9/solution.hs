{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List
import Data.Char
-- import Text.Parsec
import Control.Monad

main = do
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singleLines = lines contents
        
        -- print $ solution1 singleLines
        print $ solution2 singleLines
        -- print $ length singleLines

        hClose handle

-- hasSum :: Int -> [Int] -> Maybe (x, y)
--
solution1 = findInvalid 25 . map read
solution2 inp = do
  let ints = map read inp
  invalid <- findInvalid 25 ints
  let set = contigSet invalid ints
  return $ liftM2 (+) minimum maximum set

findInvalid :: Int -> [Int] -> Maybe Int
findInvalid pre ns = snd <$> (find check $ drop pre $ zip [0..] ns)
  where check (i, n) = case hasSum n $ take pre $ drop (i - pre) ns of
                         Just _ -> False
                         Nothing -> True

hasSum :: Int -> [Int] -> Maybe (Int, Int)
hasSum n ns = find cond ns >>= (\x -> return (x, n - x))
  where cond x = (n - x) `elem` (filter (/= x) ns)

contigSet :: Int -> [Int] -> [Int]
contigSet res ns = head $ filter ((<) 1 . length) $ map (\(i,n) -> adds [] $ drop i ns) $ zip [0..] ns
  where adds acs (n:ns)
          | (sum acs) + n == res = (n:acs)
          | (sum acs) + n >= res = []
          | (sum acs) + n <= res = adds (n:acs) ns



testInp = map (read :: String -> Int) [ "35"
          , "20"
          , "15"
          , "25"
          , "47"
          , "40"
          , "62"
          , "55"
          , "65"
          , "95"
          , "102"
          , "117"
          , "150"
          , "182"
          , "127"
          , "219"
          , "299"
          , "277"
          , "309"
          , "576"
          ]

