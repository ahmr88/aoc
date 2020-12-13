{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List
import Data.Char
import Control.Monad.Trans.State
import Data.Maybe

main = do
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singleLines = lines contents
        
        -- print $ solution1 singleLines
        print $ solution2 singleLines
        -- print $ length singleLines

        hClose handle



solution1 = length . filter (== '#') . concat . converge updateGrid

solution2 = length . filter (== '#') . concat . converge updateGrid2

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x,y) = map (mapPair (+x) (+y)) combs
  where 
    combs = filter (/= (0,0)) $ concatMap (\i -> map ((,) i) inds) inds
    inds = [-1,0,1]
    mapPair f g (a,b) = (f a, g b)

adjacent2 :: (Int, Int) -> [String] -> [(Int, Int)]
adjacent2 (x,y) grid = filter (isValid (width,height)) $ map (\abs -> fromMaybe (-1,-1) $ first abs)  
                     $ map (\d -> map (mapPair (+x) (+y)) $ dir (0,0) d) combs
  where
    first abs = find (\(a,b) -> grid !! a !! b == '#' || grid !! a !! b == 'L') 
              $ takeWhile (isValid (width, height)) abs 
    combs = filter (/= (0,0)) $ concatMap (\i -> map ((,) i) inds) inds
    inds = [-1,0,1]
    dir (a,b) (da,db) = (a + da, b + db) : dir (a + da, b + db) (da,db)
    mapPair f g (a,b) = (f a, g b)
    width = length $ head grid
    height = length $ grid

isValid (w,h) (x,y) = x >= 0 && x < w && y >= 0 && y < h

boundaryChecked :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
boundaryChecked (w,h) coord = filter (isValid (w,h)) $ adjacent coord

converge :: Eq a => (a -> a) -> a -> a
converge f x = if f x == x then x else converge f (f x)


updateGrid :: [String] -> [String]
updateGrid grid =  (map . map) action $ map (\r -> map (\c -> (r,c)) [0.. width - 1]) [0.. height - 1]
  where
    width = length $ head grid
    height = length grid
    getValue (i,j) = grid !! i !! j
    surs (x,y) = map getValue $ boundaryChecked (width, height) (x,y)
    action :: (Int,Int) -> Char
    action (x,y)
      | getValue (x,y) == 'L' && ((length $ filter (== '#') $ surs (x,y)) == 0) = '#'
      | getValue (x,y) == '#' && ((length $ filter (== '#') $ surs (x,y)) >= 4) = 'L'
      | otherwise = getValue (x,y)

updateGrid2 :: [String] -> [String]
updateGrid2 grid =  (map . map) action $ map (\r -> map (\c -> (r,c)) [0.. width - 1]) [0.. height - 1]
  where
    width = length $ head grid
    height = length grid
    getValue (i,j) = grid !! i !! j
    surs (x,y) = map getValue $ adjacent2 (x,y) grid
    action :: (Int,Int) -> Char
    action (x,y)
      | getValue (x,y) == 'L' && ((length $ filter (== '#') $ surs (x,y)) == 0) = '#'
      | getValue (x,y) == '#' && ((length $ filter (== '#') $ surs (x,y)) >= 5) = 'L'
      | otherwise = getValue (x,y)



testInp :: [String]
testInp = [ "L.LL.LL.LL"
          , "LLLLLLL.LL"
          , "L.L.L..L.."
          , "LLLL.LL.LL"
          , "L.LL.LL.LL"
          , "L.LLLLL.LL"
          , "..L.L....."
          , "LLLLLLLLLL"
          , "L.LLLLLL.L"
          , "L.LLLLL.LL"
          ]

testInp2 = [  ".......#."
            , "...#....."
            , ".#......."
            , "........."
            , "..#L....#"
            , "....#...."
            , "........."
            , "#........"
            , "...#....."
             ]
