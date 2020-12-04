{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Data.Text as T
import Data.List
import Control.Monad


main = do
        let list = []
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singleLines = lines contents

        putStrLn $ show $ solution singleLines
        hClose handle

solution :: [String] -> Maybe Int
solution singleLines = counted
  where
    counted = sequence maybeExtracted >>= return . count validPassword2
    maybeExtracted = map extractMaybe policyExtracted
    policyExtracted = map (\(pol, b) -> (passPolicy pol, b)) 
                    $ map (\str -> let (a,b) = break (== ':') str
                                   in (a, (tail . tail) b)) singleLines

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count pr = foldl' (\acc a -> if pr a then acc + 1 else acc) 0


validPassword :: ((Int, Int, Char), String) -> Bool
validPassword ((min, max, c), ps) = liftM2 (&&) (<= max) (>= min) charCount
  where charCount = count (== c) ps

validPassword2 :: ((Int, Int, Char), String) -> Bool
validPassword2 ((i, j, c), ps) = not (pred1 == pred2)
  where pred1 = (ps !! (i - 1) == c)
        pred2 = (ps !! (j - 1) == c)

extractMaybe :: (Maybe a, b) -> Maybe (a, b)
extractMaybe (Just a, b) = Just (a, b)
extractMaybe _ = Nothing

-- format: 2-5 a
passPolicy :: String -> Maybe (Int, Int, Char)
passPolicy = aux
  where
    aux pol = do
      (a,b) <- firstTwo $ words pol
      (min, max) <- firstTwo $ map (read .  T.unpack) $ T.split (== '-') $ T.pack a
      return (min, max, head b)
    firstTwo [x, y] = Just (x,y)
    firstTwo _ = Nothing

-- passPolicy str = T.split ((==) ':') str
