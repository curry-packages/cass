append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]

main :: Int -> Int -> [Int]
main x y = rev [x .. y]
