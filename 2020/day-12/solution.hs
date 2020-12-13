{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List
import Data.Char
import Control.Monad.Trans.State
import Data.Maybe
import Text.Parsec

data Dir = N | W | S | E | L | R | F deriving (Show, Eq)

type Parser = Parsec String ()

main = do
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singleLines = lines contents
        
        print $ solution1 singleLines
        -- print $ solution2 singleLines
        -- print $ length singleLines

        hClose handle

solution1 = fmap f . sequenceA . map (parse pDir "")

solution2 = undefined

pDir :: Parser (Dir, Int)
pDir = do
  dir <- choice [ char 'N' >> return N,
                  char 'W' >> return W,
                  char 'E' >> return E,
                  char 'S' >> return S,
                  char 'R' >> return R,
                  char 'L' >> return L,
                  char 'F' >> return F
                ]
  n <- read <$> many1 digit
  return (dir, n)

f :: [(Dir, Int)] -> Int
f dirs = manhattan $ foldl aux ((1,0), (0,0)) dirs
  where
    manhattan (_, (x,y)) = abs x + abs y
    aux ((h,v),(x,y)) (dir, n) = case absDir dir (h,v) n of
                                   ((h',v'), (dx,dy)) -> ((h',v'), (x + dx, y + dy))

absDir :: Dir -> (Int, Int) -> Int -> ((Int,Int),(Int,Int))
absDir N x n = (x, (0, n))
absDir S x n = (x, (0, -1 * n))
absDir E x n = (x, (n, 0))
absDir W x n = (x, (-1 * n, 0))
absDir F (h,v) n = ((h,v), (h * n, v * n))
absDir R (h,v) n = (head $ drop ((n `div` 90) `mod` 4) $ dropWhile (/= (h,v)) rotations, (0,0))
absDir L (h,v) n = absDir R (h,v) (360 - n)
rotations = [(0,1),(1,0),(0,-1),(-1,0)] ++ rotations


testInp :: [String]
testInp = [ "F10"
          , "N3"
          , "F7"
          , "R90"
          , "F11"
          ]
