import Data.Char(digitToInt)

fibo :: (Num a) => a -> a -> [a]
fibo a b = a:fibo b (a+b)

primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]



problem1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
problem2 = sum (filter even (takeWhile (\x -> x < 4000000) (fibo 0 1)))
problem4 = maximum [x * y | x <- [100..999], y <- [100..999], (show (x * y)) == (reverse (show (x * y)))]
problem6 = ((1 + 100) * 100 `div` 2) ^ 2 - ((100 * (100 + 1) * (2 * 100 + 1)) `div` 6)
problem7 = primes !! 10000
problem9 = [a * b * (1000 - a - b) | a <- [1..1000], b <- [1..1000], a < b,  let z = 1000 - a - b, a*a + b*b == z*z]
problem16 = sum (map digitToInt (show (2^1000)))
problem20 = sum (map digitToInt (show (product [1..100])))
problem25 = length (takeWhile (\x -> (length (show x)) < 1000) (fibo 0 1))
problem48 = (sum [x^x | x <- [1..1000]]) `mod` 10000000000
