data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float
             deriving (Show)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) =
            (abs $ x2 - x1) * (abs $ y2 - y1)

data Vector a = Vector a a a deriving (Show, Eq)
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) =
                Vector (i+l) (j+m) (k+n)

dot :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dot` (Vector l m n) =
            i*l + j*m + k*n

data Person = Person { first :: String,
                       last  :: String,
                       age   :: Int }
              deriving (Eq, Show)

data List a = EmptyList |
              Cons a (List a)
              deriving (Show, Eq, Ord)

data Tree a = EmptyTree |
              Node a (Tree a) (Tree a)
              deriving (Show)

leaf :: a -> Tree a
leaf x = Node x EmptyTree EmptyTree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = leaf x
insert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (insert x left) right
    | x > a = Node a left (insert x right)

search :: (Ord a) => a -> Tree a -> Bool
search x EmptyTree = False
search x (Node a left right)
    | x == a = True
    | x < a = search x left
    | x > a = search x right

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) =
        Node (f x) (fmap f left) (fmap f right)
