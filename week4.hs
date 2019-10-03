collatz :: [Int] -> [Int]
collatz [] = []
collatz (x:xs)
    | (x `mod` 2) == 0 = (x `div` 2):(collatz xs)
    | (x `mod` 2) == 1 = (3*x + 1):(collatz xs)



zap :: [Int] -> [Int]
zap [] = []
zap [x,y]
    | x == y = [x]
    | otherwise = [x,y]
zap (x:y:xs)
    | x == y = zap (x:xs)
    | otherwise = x:(zap (y:xs))

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)
   | x `elem` xs   = rmdups xs
   | otherwise     = x : rmdups xs

breakUp :: [(Int, Int)] -> [Int]
breakUp [] = []
breakUp [(x,y)] = [x,y]
breakUp ((x,y):xs) = x:y:(breakUp xs)

twoSort :: [Int] -> [Int]
twoSort [] = []
twoSort [x,y]
    | (x < y) = [y,x]
    | otherwise = [x,y]
twoSort (x:y:xs)
    | (x < y) = (y:(twoSort (x:xs)))
    | otherwise = (x:(twoSort(y:xs)))

secondLast :: [Int] -> Int
secondLast [x,y] = x
secondLast (x:xs) = secondLast (xs)

remRunnerUp :: [Int] -> [Int]
remRunnerUp [] = []
remRunnerUp [x,y]
    | (x < y) = [y]
    | (y < x) = [x]
    | otherwise = [x,y]
remRunnerUp [x] = [x]
remRunnerUp (x:xs) = remRunnerUpAux (x:xs) secondLargest
    where secondLargest = secondLast (twoSort (twoSort (rmdups (x:xs))))

remRunnerUpAux :: [Int] -> Int -> [Int]
remRunnerUpAux [] _ = []
remRunnerUpAux [x] secondLargest
    | (x==secondLargest) = []
    | otherwise = [x]
remRunnerUpAux (x:xs) secondLargest
    | (x == secondLargest) = remRunnerUpAux xs secondLargest
    | otherwise = (x:(remRunnerUpAux xs secondLargest))