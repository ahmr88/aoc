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
    counted = sequence maybeExtracted >>= return . count validPassword
    maybeExtracted = map extractMaybe policyExtracted
    policyExtracted = map (\(pol, b) -> (passPolicy pol, b)) 
                    $ map (\str -> let (a,b) = break (== ':') str
                                   in (a, tail b)) singleLines

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count pr = foldl' (\acc a -> if pr a then acc + 1 else acc) 0

validPassword ((min, max, c), ps) = liftM2 (&&) (<= max) (>= min) charCount
  where charCount = count (== c) ps

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




