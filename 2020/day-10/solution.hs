{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List
import Data.Char
import Control.Monad

main = do
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singleLines = lines contents
        
        -- print $ solution1 singleLines
        -- print $ solution2 singleLines
        -- print $ length singleLines

        hClose handle


solution1 = undefined

solution2 inp = undefined

testInp = [ "nop +0"
          , "acc +1"
          , "jmp +4"
          , "acc +3"
          , "jmp -3"
          , "acc -99"
          , "acc +1"
          , "jmp -4"
          , "acc +6"
          ]
