{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List
import Data.Char
import Control.Monad

main = do
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singleLines = lines contents
        
        print $ solution1 singleLines
        print $ solution2 singleLines
        -- print $ length singleLines

        hClose handle


solution1 inp = count (id) $ map (passHasFields passKey) $ parsePasses inp

solution2 = count (id) . map isPassValid . parsePasses
  where
    isPassValid = liftM2 (&&) (all validateValue) (passHasFields passKey)

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count pr = foldl' (\acc a -> if pr a then acc + 1 else acc) 0

parsePasses = (map . map) (tailSnd . break (== ':'))
            . map (concatMap words) 
            . filter (/= [""]) 
            . groupBy (\a b -> a /= "" && b /= "")
  where
    tailSnd (a, b) = (a, tail b)

passHasFields :: [String] -> [(String, String)] -> Bool
passHasFields key pass = all (flip elem $ passFields) key
  where passFields = map fst pass

passKey :: [String]
passKey = ["ecl", "eyr" , "byr", "iyr", "pid", "hcl", "hgt"]

validateValue :: (String, String) -> Bool
validateValue (field, value) = case field of
                                 "byr" -> all isDigit value && (intBetween 1920 2002 $ read value)
                                 "iyr" -> all isDigit value && (intBetween 2010 2020 $ read value)
                                 "eyr" -> all isDigit value && (intBetween 2020 2030 $ read value)
                                 "ecl" -> elem value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                                 "pid" -> length value == 9 && all isDigit value
                                 "hcl" -> (head value) == '#' 
                                       && (length $ tail value) == 6 
                                       && (all (liftM2 (||) isDigit (flip elem $ ['a'..'f'])) $ tail value)
                                 "hgt" -> validHeight $ span isDigit value
                                 _ -> True
  where intBetween x y n = x <= n && n <= y
        validHeight ([], _) = False
        validHeight (n, "cm") = 150 <= (read n) && (read n) <= 193
        validHeight (n, "in") = 59 <= (read n) && (read n) <= 76
        validHeight (_, _) = False

testInp = ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd","byr:1937 iyr:2017 cid:147 hgt:183cm","","iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884","hcl:#cfa07d byr:1929","","hcl:#ae17e1 iyr:2013","eyr:2024","ecl:brn pid:760753108 byr:1931","hgt:179cm","","hcl:#cfa07d eyr:2025 pid:166559648","iyr:2011 ecl:brn hgt:59in"]
