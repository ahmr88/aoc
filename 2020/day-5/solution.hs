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


solution1 = maximum . map (seatId . parsePos) 

solution2 inp = filter (not . (flip elem) mapToId) seats
  where seats = [minimum mapToId..maximum mapToId]
        mapToId = map (seatId . parsePos) inp

parsePos :: String -> (Int, Int)
parsePos = aux . splitAt 7
  where aux (a,b) = (decodePos $ map (== 'B') a, decodePos $ map (== 'R') b)


decodePos :: [Bool] -> Int
decodePos bin =  foldl addRow 0 $ zip [0..] bin
  where
    addRow y (i, True) = y + 2^((inputLength-1) - i)
    addRow y (_, _) = y
    inputLength = length bin

seatId :: (Int, Int) -> Int
seatId (x,y) = x * 8 + y

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count pr = foldl' (\acc a -> if pr a then acc + 1 else acc) 0


testInp = [ "BFFFBBFRRR"
            ,"FFFBBBFRRR"
            ,"BBFFBBFRLL"
          ]
