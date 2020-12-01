doubleMe x = x + x

doubleUs x y = 2 * x + 2 * y

doubleSmallNumber x = if x > 100
    then x
    else 2 * x

doubleSmallNumber' x = (if x > 100 then x else 2*x) + 1

-- Each odd number greater than 10 -> BANG!, each odd number less than 10 -> BOOM!
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- Custom version of length function
length' xs = sum [1 | _ <- xs]

-- Example of explicit type declaration
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase str = [c | c <- str, c `elem` ['A'..'Z']]

-- Explicit type declaration of function with multiple parameters
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Check if input to function is 7
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number 7!"
lucky x = "Sorry, you're out of luck!"

-- Add together two vectors in 2D space
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Functions similar to fst and snd that extract first, second, or third component of triple
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z


-- Custom head function, using pattern matching against list
head' :: [a] -> a
head' [] = error "You can't call head on an empty list!"
head' (x:_) = x

-- Using patterns: get first letter in string
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- Using guards for returns
numBetween :: (Real a) => a -> String
numBetween x
    | x <= 10 = "Num less than 10"
    | x <= 20 = "Num between 10 and 20"
    | otherwise = "Num greater than 20"


-- Create function as infix
myComp :: (Ord a) => a -> a -> Ordering
a `myComp` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT


-- Example function using where and pattern match
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny     = "Underweight"
    | bmi <= normal     = "Normal"
    | bmi <= fat        = "Fat"
    | otherwise         = "Obese"
    where
        bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)


-- Get first and last name, return initials
initials :: String -> String -> String
initials fn ln = [f] ++ ". " ++ [l] ++ "."
    where
        (f:_) = fn
        (l:_) = ln

-- Using case expressions
describeList :: [a] -> String
describeList xs = "The list is " ++
    case xs of
        [] -> "empty"
        [x] -> "a singleton list"
        xs -> "a longer list"

-- Alternative of above:
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where
        what [] = "empty"
        what [x] = "a singleton list"
        what xs = "a longer list"