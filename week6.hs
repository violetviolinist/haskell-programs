dropOdds :: Int -> Int
dropOdds n
    | n < 10 && n `mod` 2 == 0 = n
    | n < 10 && n `mod` 2 == 1 = 0
    | (n `mod` 10) `mod` 2 == 1 = dropOdds (n `quot` 10)
    | (n `mod` 10) `mod` 2 == 0 = (dropOdds (n `quot` 10)) * 10 + (n `mod` 10)

moreZeros :: Int -> Bool
moreZeros n = moreZerosAux n 0 0

moreZerosAux :: Int -> Int -> Int -> Bool
moreZerosAux n zero one
    | n < 2 && n == 0 && ((zero+1) > one) = True
    | n < 2 && n == 1 && (zero > (one+1)) = True
    | n < 2 = False
    | n `mod` 2 == 0 = moreZerosAux (n `quot` 2) (zero+1) one
    | n `mod` 2 == 1 = moreZerosAux (n `quot` 2) zero (one+1)

binToTer :: Int -> Int
binToTer n = decToTer (binToDec n)

binToDec :: Int -> Int
binToDec n = binToDecAux n 0

binToDecAux :: Int -> Int -> Int
binToDecAux n pow
    | n == 0 = 0
    | otherwise = (binToDecAux (n `quot` 10) (pow+1)) + ((2^pow) * (n `mod` 10))

decToTer :: Int -> Int
decToTer n
    | n < 3 = n
    | otherwise = (decToTer (n `quot` 3)) * 10 + n `mod` 3

palindrome :: Int -> Bool
palindrome n
    | n == (reverseInt n) = True
    | otherwise = False

reverseInt :: Int -> Int
reverseInt x | x < 0     = 0 - (read . reverse . tail . show $ x)
             | otherwise = read . reverse . show $ x