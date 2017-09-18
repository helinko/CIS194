--Ex1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--Ex2
-- takes a list and returns a list with one more fibonacci number
fib2 :: [Integer] -> [Integer]
fib2 [] = [0]
fib2 (_:[]) = [1, 0]
fib2 (x:y:xs) = (x + y):x:y:xs

-- For each Fibonacci number there's one call of fib2, which has one addition -> O(n) additions
fibs2 :: [Integer]
fibs2 = map head (iterate fib2 [0])

testFib :: Bool
testFib = take 25 fibs1 == take 25 fibs2

-- Ex3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h:(streamToList t)

instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))
