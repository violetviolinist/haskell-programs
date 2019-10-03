subSeq :: String -> String -> Bool
subSeq [] _ = True
subSeq (x:xs) [] = False
subSeq [x] [y]
    | x == y = True
    | otherwise = False
subSeq [y] (x:xs)
    | x == y = True
    | otherwise = subSeq [y] ((head xs):(tail xs))
subSeq (x:xs) [y] = False
subSeq (x1:xs1) (x2:xs2)
    | x1 == x2 = subSeq ((head xs1):(tail xs1)) ((head xs2):(tail xs2))
    | otherwise = subSeq (x1:xs1) ((head xs2):(tail xs2))

subWord :: String -> String -> Bool
subWord [] _ = True
subWord (x:xs) [] = False
subWord [x] (y:ys)
    | x == y = True
    | otherwise = False
subWord (x:xs) [y] = False
subWord (x:xs) (y:ys)
    | subWordAux (x:xs) (y:ys) == True = True
    | otherwise = subWord (x:xs) ((head ys):(tail ys))

subWordAux :: String -> String -> Bool
subWordAux (x:xs) [] = False 
subWordAux [x] (y:ys)
    | x == y = True
    | otherwise = False
-- subWordAux [x] (y:ys)
--     | x == y = True
--     | otherwise = False
subWordAux (x:y:ys) [z] = False
subWordAux (x:xs) (y:ys)
    | x == y = subWordAux ((head xs):(tail xs)) ((head ys):(tail ys))
    | otherwise = False

isMatrix :: [[a]] -> Bool
-- isMatrix [] = 0
isMatrix [[x]] = True
isMatrix [] = False
isMatrix ([]:xs) = False
isMatrix [(x:xs)] = True
isMatrix ((x:xs):ys)
    | isMatrixAux ((x:xs):ys) == -1 = False
    | otherwise = True 

isMatrixAux :: [[a]] -> Int
isMatrixAux [] = -1
isMatrixAux [(x:xs)] = length (x:xs)
isMatrixAux ([]:xs) = -1
isMatrixAux ((x:xs):ys)
    | cur_length == isMatrixAux ((head ys):(tail ys)) = cur_length
    | otherwise = -1
    where cur_length = length (x:xs)

isSquareMatrix :: [[a]] -> Bool
-- isSquareMatrix [] = 0
isSquareMatrix [[x]] = True
isSquareMatrix [] = False
isSquareMatrix ([]:xs) = False
isSquareMatrix ((x:xs):ys)
    | isSquareMatrixAux ((x:xs):ys) == length ((x:xs):ys) = True
    | otherwise = False 

isSquareMatrixAux :: [[a]] -> Int
isSquareMatrixAux [] = -1
isSquareMatrixAux [(x:xs)] = length (x:xs)
isSquareMatrixAux ([]:xs) = -1
isSquareMatrixAux ((x:xs):ys)
    | cur_length == isSquareMatrixAux ((head ys):(tail ys)) = cur_length
    | otherwise = -1
    where cur_length = length (x:xs)

addable :: [[a]] -> [[a]] -> Bool
addable [[]] [[]] = False
addable [] _ = False
addable _ [] = False
addable [[]] _ = False
addable _ [[]] = False
addable ([]:xs) _ = False
addable _ ([]:xs) = False
addable ((x1:x1s):y1s) ((x2:x2s):y2s)
    | (length ((x1:x1s):y1s) == length ((x2:x2s):y2s) && columnLength1 /= -1 && columnLength1 == columnLength2) = True
    | otherwise = False
    where columnLength1 = isMatrixAux ((x1:x1s):y1s)
          columnLength2 = isMatrixAux ((x2:x2s):y2s)

addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices [(x:xs)] [(y:ys)] = [addRows (x:xs) (y:ys)]
addMatrices ((x1:x1s):y1s) ((x2:x2s):y2s) = ((addRows (x1:x1s) (x2:x2s)):(addMatrices y1s y2s))

addRows :: [Int] -> [Int] -> [Int]
addRows [x] [y] = [x+y]
addRows (x:xs) (y:ys) = ((x+y):(addRows xs ys))

multiplyable :: [[a]] -> [[a]] -> Bool
multiplyable ((x1:x1s):y1s) ((x2:x2s):y2s)
    | columnlength1 == length ((x2:x2s):y2s) = True
    | otherwise = False
    where columnlength1 = isMatrixAux ((x1:x1s):y1s)

multiplyMatrices:: Num a => [[a]] -> [[a]] -> [[a]]
multiplyMatrices [] _  = error "left matrix is empty"
multiplyMatrices _ []  = error "right matrix is empty"
multiplyMatrices us vs = map (mult [] vs) us
    where
    mult xs [] _ = xs
    mult xs _ [] = xs
    mult [] (zs:zss) (y:ys) = mult (map (\v -> v*y) zs) zss ys
    mult xs (zs:zss) (y:ys) = mult (zipWith (\u v -> u+v*y) xs zs) zss ys