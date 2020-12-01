data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Function to compute surface area of shape
surfaceArea :: Shape -> Float
surfaceArea (Circle _ r) = pi * r ^ 2
surfaceArea (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- "Nudge" the shape by an amount along the x and y axes
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


----------------- Personal Records ------------------
data Person = Person
    {
        firstName :: String,
        lastName :: String,
        age :: Int,
        height :: Float,
        weight :: Float,
        phoneNumber :: String
    } deriving (Show)


-- 3D Vector type
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)



--------- Recursive data structures ----------------

-- Implement our own list type
--data List' a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- Or, using an infix constructor:
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- Create function that adds two Lists together
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Use:
-- let a = 3 :-: 4 :-: Empty
-- let b = 5 :-: Empty
-- a .++ b


--------- Implement binary search tree -------------

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Function to crate singleton tree
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- Function to insert an element into a tree. If smaller, go left; if larger, go right.
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

-- Function to check if element is in tree.
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right


----------- More on typeclasses -----------

-- Create simple data type
data TrafficLight = Red | Yellow | Green

-- Create an instance of Eq ("=="/"/=") for TrafficLight
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- Create instance of Show for TrafficLight
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- Implement a yes/no typeclass (like checking bool on non-bool value in JavaScript)
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id -- "id" takes parameter and returns the same thing

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

-- Create an "if"-like function for yesno
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult