{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List
import Data.Char
import Text.Parsec
import Control.Monad

data Command = Acc Int | Jump Int | Nop Int deriving (Show, Eq)

type Parser = Parsec String ()


main = do
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singleLines = lines contents
        
        -- print $ solution1 singleLines
        -- print $ solution2 singleLines
        -- print $ length singleLines

        hClose handle


solution1 = findLoop 0 [0] . zip [0..] . rights . map (parse parseRules "")

solution2 = undefined

findLoop :: Int -> [Int] -> [(Int, Command)] -> Int
findLoop v (i:is) prog
  | i >= length prog = v
  | i `elem` is = v
  | otherwise = case prog !! i of
                  (_, Nop _) -> findLoop v ((i + 1) : i : is) prog
                  (_, Acc x) -> findLoop (v + x) ((i + 1) : i : is) prog
                  (_, Jump x) -> findLoop v ((i + x) : i : is) prog

parseRules :: Parser Command
parseRules = do
  x <- pInstruction
  skipMany space
  arg <- pSigned
  return $ x arg

pSigned :: Parser Int
pSigned = read <$> ((char '-' >>= (\c -> (:) c <$> many digit)) <|> (char '+' >> many digit))

pString :: String -> Parser String
pString str = sequenceA $ map char str


pInstruction :: Parser (Int -> Command)
pInstruction = do
  inst <- pString "acc" <|> pString "jmp" <|> pString "nop"
  case inst of 
    "acc" -> return Acc
    "jmp" -> return Jump
    "nop" -> return Nop

rights :: [Either a b] -> [b]
rights [] = []
rights (Right b: xs) = b : rights xs
rights (_: xs) = rights xs

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
