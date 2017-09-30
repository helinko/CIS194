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
streamToList (Cons x xs) = x:(streamToList xs)

instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))

instance Eq a => Eq (Stream a) where
  xs == ys = take 50 (streamToList xs) == take 50 (streamToList ys)
-- Ex4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

test1 :: Bool
test1 = streamMap (+3) (streamRepeat 22) == streamRepeat 25

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Ex 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- an ugly implementation from the definition (for test reference)

-- for powers 2^x = y this is a list of (x,y) pairs
powersOfTwos :: [(Integer, Integer)]
powersOfTwos = zip (iterate (+1) 0) (map (2^) (iterate (+1) 0)) 

candidates :: Integer -> [(Integer, Integer)]
candidates n = reverse (takeWhile (\(_,y) -> y <= n) powersOfTwos)

largestPowerOf2 :: Integer -> Integer
largestPowerOf2 n = fst (head (dropWhile (\(_,y) -> mod n y /= 0) (candidates n)))

ruler1 :: Stream Integer
ruler1 = streamMap largestPowerOf2 (streamMap (+1) nats)

-- the more efficient version

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) =
  (Cons x (Cons y (interleaveStreams xs ys)))

test2 :: Bool 
test2 = interleaveStreams (streamRepeat 0) (streamRepeat 1) ==
  streamMap ((flip mod) 2) (streamFromSeed (+1) 0)

-- This gets evaluated too early and stack overflows
rulerRec :: Integer -> Stream Integer
rulerRec n = interleaveStreams (streamRepeat n) (rulerRec (n + 1))

  -- interleaveStreams (streamRepeat 0) 
  --                   (interleaveStreams (streamRepeat 1)
  --                                      (interleaveStreams (streamRepeat 2)

test3 :: Bool
test3 = ruler1 == rulerRec 0
