import System.IO
import Control.Monad

main :: IO ()
main = do
    let list = []
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = f singlewords

    putStrLn $ show $ (\(a,b,c) -> a * b * c) $ mult3Of2020 list
    
    print list
    hClose handle

multOf2020 :: [Int] -> Int -> (Int, Int)
multOf2020 ls sum = foldl aux (0,0) ls
  where
    aux (f, s) b 
      | f == 0 && s == 0 = case filter ((==) sum . (+) b) ls of
                            [] -> (f,s)
                            (x:xs) -> (b,x)
      | otherwise = (f,s)

mult3Of2020 :: [Int] -> (Int, Int, Int)
mult3Of2020 ls = foldl aux (0,0,0) ls
  where
    aux (f,s,t) b
      | f == 0 && s == 0 && t == 0 = case multOf2020 ls (2020 - b) of
                                       (0,0) -> (f,s,t)
                                       (x,y) -> (b,x,y)
      | otherwise = (f,s,t)

f :: [String] -> [Int]
f = map read
