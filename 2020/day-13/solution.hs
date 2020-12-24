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
solution1 = undefined
solution2 inp = summed `mod` nn
  where
    parsed = parseInp inp
    nn = getNN parsed
    summed = sum $ map (comp nn) parsed


parseInp :: [String] -> [(Int, Int)]
parseInp = map (\(a,b) -> (a, read b)) . filter ((>0) . length . snd) .  zip [0..] . (map . filter) (\c -> c /= 'x') . splitWhen (== ',') . head . drop 1 
-- parseInp = zip [0..] . read . (concatMap . filter) (\c -> c /= 'x') . splitWhen (== ',') . head . drop 1 

exteuc :: (Int, Int) -> (Int, Int, Int)
exteuc (a, 0) = (a, 1, 0)
exteuc (a, b) = let (d, x', y') = exteuc (b, a `mod` b) in (d,y',x' - (a `div` b) * y')

modinv :: Int -> Int -> (Int)
modinv a b = if inv < 0 then b + inv else inv
  where (_, inv, _) = exteuc (a,b)

comp ::  Int -> (Int, Int) -> Int
comp nn (i, n) = ai * yi * zi
  where ai = (n - i)
        yi = floor $ (fromIntegral nn) / (fromIntegral n)
        zi = modinv yi n

getNN :: [(Int, Int)] -> Int
getNN = product . map snd 

splitWhen :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitWhen f l = foldr (\x acc -> if f x then [] : acc else (x : head acc) : tail acc) [[]] l

testInp :: [(Int, Int)]
testInp = [ (0,7 )
          , (1,13)
          , (4,59)
          , (6,31)
          , (7,19)
          ]

testInp2 :: [(Int, Int)]
testInp2= [ (2,3)
          , (1,5)
          , (1,7)
          ]
