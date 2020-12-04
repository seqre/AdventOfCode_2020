module Main (main) where

import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Text.Read
import Text.Regex


main :: IO()
main = do
        raw_input <- readFile "input.txt"
        let passports = endBy "\n\n" raw_input
        let cln_pass = map (map (splitAt 4) . filter (/="") . splitOneOf "\n ") passports
        print $ take 10 cln_pass
        print ("Sol 1: " ++ (show $ sol1 cln_pass))
        print ("Sol 2: " ++ (show $ sol2 cln_pass))

sol1 :: [[(String,String)]] -> Int
sol1 [] = 0
sol1 (pass:rest) = let check_pass :: [(String,String)] -> Int
                       check_pass [] = 0
                       check_pass x = let pass_fields = sort $ map (init . fst) x
                                          fields_less = ["byr","ecl","eyr","hcl","hgt","iyr","pid"]
                                          fields = ["byr","cid","ecl","eyr","hcl","hgt","iyr","pid"]
                                       in if (pass_fields == fields || pass_fields == fields_less)
                                          then 1 else 0
                   in  check_pass pass + sol1 rest


sol2 :: [[(String,String)]] -> Int
sol2 [] = 0
sol2 (pass:rest) = let check_pass :: [(String,String)] -> Int
                       check_pass [] = 0
                       check_pass x = let pass_fields = sort $ map (init . fst) x
                                          fields_less = ["byr","ecl","eyr","hcl","hgt","iyr","pid"]
                                          fields = ["byr","cid","ecl","eyr","hcl","hgt","iyr","pid"]
                                          funcs = M.fromList [("byr",checkBYR),("iyr",checkIYR),("eyr",checkEYR),("hgt",checkHGT),("hcl",checkHCL),("ecl",checkECL),("pid",checkPID),("cid",checkCID)]
                                          temp_func :: Bool -> (String,String) -> Bool
                                          temp_func acc (f,v) = acc && M.findWithDefault (\x -> False) (init f) funcs v
                                          fields_check = foldl temp_func True x
                                       in if (pass_fields == fields || pass_fields == fields_less) && fields_check
                                          then 1 else 0
                   in  check_pass pass + sol2 rest

checkBYR :: String -> Bool
checkBYR x = let m = readMaybe x::Maybe Int
                 n = fromMaybe 0 m
             in  isJust m && length x == 4 && 1920 <= n && n <= 2020

checkIYR :: String -> Bool
checkIYR x = let m = readMaybe x::Maybe Int
                 n = fromMaybe 0 m
             in  isJust m && length x == 4 && 2010 <= n && n <= 2020

checkEYR :: String -> Bool
checkEYR x = let m = readMaybe x::Maybe Int
                 n = fromMaybe 0 m
             in  isJust m && length x == 4 && 2020 <= n && n <= 2030

checkHGT :: String -> Bool
checkHGT x = let (num,unit) = splitAt (length x - 2) x
                 m = readMaybe num::Maybe Int
                 n = fromMaybe 0 m
             in if isJust m && unit /= "" && unit == "in" then 59 <= n && n <= 76
                                                          else 150 <= n && n <= 193

checkHCL :: String -> Bool
checkHCL x = let regex = mkRegex "[a-f0-9]{6}"
             in  head x == '#' && isJust (matchRegex regex (tail x))

checkECL :: String -> Bool
checkECL x = elem x ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

checkPID :: String -> Bool
checkPID x = length x == 9

checkCID :: String -> Bool
checkCID _ = True
