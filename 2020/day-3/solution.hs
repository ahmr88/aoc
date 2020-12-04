{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.List


main = do
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singleLines = lines contents
        
        -- print $ solution1 singleLines
        print $ solution2 singleLines
        -- print $ length singleLines

        hClose handle


solution1 :: [String] -> Int
solution1 inp = count (== '#') $ getPath inp (3,1)

solution2 inp = product $ map (count (== '#') . getPath inp) slopes
  where slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count pr = foldl' (\acc a -> if pr a then acc + 1 else acc) 0

coords :: (Int, Int) -> [(Int, Int)]
coords (x, y) = aux (0,0)
  where 
    aux (a, b) = (a, b) : aux (a+x, b+y)

getPath :: [String] -> (Int, Int) -> String
getPath inp (mx, my) = foldl aux [] $ take maxRow $ coords (mx, my)
  where
    aux path (x,y) = (inp !! y  !! (x `mod` maxCol)) : path
    maxCol = length $ head inp
    maxRow = ceiling $ (fromIntegral $ length inp) / (fromIntegral my)


testInp = ["..##.......","#...#...#..",".#....#..#.","..#.#...#.#",".#...##..#.","..#.##.....",".#.#.#....#",".#........#","#.##...#...","#...##....#",".#..#...#.#"]
