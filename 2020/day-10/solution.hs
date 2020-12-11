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


solution1 inp = fstTimesThird $ getDiffs (0,0,0) $ 0:sorted
  where
    sorted = sort $ map read inp
    fstTimesThird (x,_,y) = x * y

solution2 inp = f $ map read inp

f :: [Int] -> Int
f ns = fst $ runState aux (0, sortReved, [])
  where 
    aux = do
      (paths, ns', vis) <- get
      case ns' of
        [] -> return paths
        (x:xs) -> do
          case childNodes x of
            [] -> put (1, xs, (x, 1) : vis)
            chs -> put (sumChldn vis chs, xs, (x, sumChldn vis chs):vis)
          aux
    sumChldn vis chs = sum $ map (\c -> fromMaybe 0 $ lookup c vis) chs
    childNodes x = filter ((x + 3) >=) $ take 3 $ drop 1 $ dropWhile (/= x) sorted
    sortReved = reverse sorted
    sorted = sort (0:ns)


getDiffs :: (Int, Int, Int) -> [Int] -> (Int, Int, Int)
getDiffs (d1,d2,d3) (x:[]) = (d1,d2,d3 + 1)
getDiffs (d1,d2,d3) (x:x':xs) = getDiffs (added $ getDiff x' x) (x':xs)
  where added (d1',d2',d3') = (d1 + d1', d2 + d2', d3 + d3')

getDiff :: Int -> Int -> (Int, Int, Int)
getDiff x' x
  | x' - x == 1 = (1,0,0)
  | x' - x == 2 = (0,1,0)
  | x' - x == 3 = (0,0,1)
  | otherwise = (0,0,0)



testInp :: [Int]
testInp = [ 16
          , 10
          , 15
          , 5
          , 1
          , 11
          , 7
          , 19
          , 6
          , 12
          , 4
          ]

testInp2 :: [Int]
testInp2 = [ 28
           , 33
           , 18
           , 42
           , 31
           , 14
           , 46
           , 20
           , 48
           , 47
           , 24
           , 23
           , 49
           , 45
           , 19
           , 38
           , 39
           , 11
           , 1
           , 32
           , 25
           , 35
           , 8
           , 17
           , 7
           , 9
           , 4
           , 2
           , 34
           , 10
           , 3
           ]
