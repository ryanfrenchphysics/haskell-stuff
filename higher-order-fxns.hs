-- Infix function with parentheses
-- Ex: divideByTen 20 == 20/10 == (/10) 20
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- Create function that takes a function and applies it twice to something
-- To use: applyTwice (+3) 10 --> 16
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


-- Implement the zipWith function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Implement the flip function, which takes a function and returns a function with the first two arguments flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where
        g x y = f y x

-- An easier version of the above:
flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f y x = f x y

-- Implement a filter function
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs


-- Function to tell us: for starting numbers 1-100, how many have Collatz chains with a length greater than x?
-- Collatz chain: if even, divide by 2. If odd, *3 and +1.
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int -> Int
numLongChains numChains = length (filter' isLong (map chain [1..100]))
    where
        isLong xs = length xs > numChains