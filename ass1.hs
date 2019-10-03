f :: Int -> Int
f n = g n 0

g :: Int -> Int -> Int
g n a
   | n == 0	        = a
   | otherwise = g q (10*a + r)
	   where
		   q = div n 10
		   r = mod n 10