module Main (main) where

import Data.List
import Data.List.Split

type Batch = (Int, Int, Char, String)

getBoundaries :: Batch -> (Int,Int)
getBoundaries (l, u, _, _) = (l, u)

getLetter :: Batch -> Char
getLetter (_, _, l, _) = l

getPass :: Batch -> String
getPass (_, _, _, p) = p

parseToBatch :: String -> Batch
parseToBatch s = let l = splitOneOf "- :" s
                 in  (read (l !! 0)::Int, read (l !! 1)::Int, (l !! 2) !! 0, l !! 4)


sol1 :: [Batch] -> Int
sol1 [] = 0
sol1 l = let check :: Batch -> Bool
             check (l,u,c,p) = let num = foldl (\acc el -> if el == c then acc + 1 else acc) 0 p
                               in  l <= num && num <= u
         in  foldl (\acc el -> if check el then acc + 1 else acc) 0 l

sol2 :: [Batch] -> Int
sol2 [] = 0
sol2 l = let check :: Batch -> Bool
             check (x,y,c,p) = (p !! (x - 1) == c) /= (p !! (y - 1) == c)
         in foldl (\acc el -> if check el then acc + 1 else acc) 0 l

main :: IO()
main = do
    raw_input <- readFile "input.txt"
    let input = map parseToBatch $ lines $ raw_input
    print ("Sol 1: " ++ (show $ sol1 input))
    print ("Sol 2: " ++ (show $ sol2 input))


