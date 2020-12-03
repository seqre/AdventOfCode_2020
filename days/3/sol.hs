module Main (main) where


sol1 :: [[Char]] -> Int -> Int
sol1 [] _ = 0
sol1 (top:rest) i = let col = mod (i+3) (length top)
                    in  if top !! col == '#' then 1 + sol1 rest col else sol1 rest col

sol2 :: [[Char]] -> Int
sol2 [] = 0
sol2 l = let traverse :: [[Char]] -> Int -> Int -> Int -> Int
             --          slope       right  down   index  result
             traverse [] _ _ _ = 0
             traverse (top:rest) r d i = let col = mod (i+r) (length top)
                                             slide = snd $ splitAt (d-1) rest
                                         in if top !! col == '#' then 1 + traverse slide r d col
                                                                 else traverse slide r d col
        in traverse l 1 1 0 * traverse l 3 1 0 * traverse l 5 1 0 * traverse l 7 1 0 * traverse (tail l) 1 2 0



main :: IO()
main = do
        raw_input <- readFile "input.txt"
        let slope = lines raw_input
        print ("Sol 1: " ++ (show $ sol1 (tail slope) 0))
        print ("Sol 2: " ++ (show $ sol2 (tail slope)))
