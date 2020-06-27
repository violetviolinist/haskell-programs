breakUp :: [(String, Int)] -> [(String, [Int])]
breakUp [] = []
breakUp ((x,y):zs) = srt (isort ((x,y):zs)) "" []

insert :: (String, Int) -> [(String, Int)] -> [(String, Int)]
insert (x,y) [] = [(x,y)]
insert (x,y) ((a,b):zs)
    | (x <= a) = (x,y):(a,b):zs
    | otherwise =  (a,b):(insert (x,y) zs)

isort :: [(String, Int)] -> [(String, Int)]
isort [] = []
isort ((x,y):xs) = insert (x,y) (isort xs)

srt :: [(String, Int)] -> String -> [(String, [Int])] -> [(String, [Int])]
srt [] prev ((a,(b:bs)):cs) = ((a,(b:bs)):cs)
srt ((x,y):zs) prev [] = filtersort (srt zs x [(x,[y])])
srt ((x,y):zs) prev ((a,(b:bs)):cs)
    | prev == "" = filtersort (srt zs x [(x,[y])])
    | prev == x = filtersort (srt zs x (addToLast2 y ((a,(b:bs)):cs)))
    |otherwise = filtersort (srt zs x ((a, (b:bs)):cs++[(x, [y])]))

filtersort :: [(String, [Int])] -> [(String, [Int])]
filtersort [] = []
filtersort ((a,(b:bs)):cs) = ((a, quicksort (b:bs)): filtersort cs)

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

addToLast2 :: Int -> [(String, [Int])] -> [(String, [Int])]
addToLast2 x [] = []
addToLast2 x [ (a,(b:bs)) ] = [(a, (b:(bs++[x])))]
addToLast2 x ((a,(b:bs)):cs) = ((a, (b:bs)):(addToLast2 x cs))

ascents :: [Int] -> [[Int]]
ascents [] = []
ascents (x:xs) = ascentsAux (x:xs) 0 [[]]

ascentsAux :: [Int] -> Int -> [[Int]] -> [[Int]]
ascentsAux [] prev ((y:ys):zs) = ((y:ys):zs)
ascentsAux (x:xs) prev [[]] = filter2 (ascentsAux xs x [[x]])
ascentsAux (x:xs) prev ((y:ys):zs)
    | (x >= prev) = filter2 (ascentsAux xs x (addToLast x ((y:ys):zs)))
    | otherwise = filter2 (ascentsAux xs x ((y:ys):zs++[[x]]))

addToLast :: Int -> [[Int]] -> [[Int]]
addToLast x [[y]] = [[y, x]]
addToLast x [(y:ys)] = [(y:ys++[x])]
addToLast x (y:ys) = (y:(addToLast x ys))

filter2 :: [[Int]] -> [[Int]]
filter2 [] = []
filter2 [[]] = [[]]
filter2 ([x]:xs) = filter2 xs
filter2 ((x:xs):ys) = ((x:xs):(filter2 ys))