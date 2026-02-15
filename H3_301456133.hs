-- 1
data List a = Empty | Cons a (List a) deriving (Show)

listZip :: List a -> List b -> List (a,b)
listZip Empty _ = Empty
listZip _ Empty = Empty
listZip (Cons x xs) (Cons y ys) = Cons (x, y) (listZip xs ys)

-- 2
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert v EmptyTree = Node v EmptyTree EmptyTree
insert v (Node val left right)
    | v < val   = Node val (insert v left) right
    | v > val   = Node val left (insert v right)
    | otherwise = Node val left right

-- 3
data Nat = Zero | Succ Nat deriving (Show)

natPlus :: Nat -> Nat -> Nat
natPlus Zero n = n
natPlus (Succ m) n = Succ (natPlus m n)

natMult :: Nat -> Nat -> Nat
natMult Zero _ = Zero
natMult (Succ m) n = natPlus n (natMult m n)

-- 4
instance Eq a => Eq (Tree a) where
    EmptyTree == EmptyTree = True
    (Node v1 l1 r1) == (Node v2 l2 r2) = (v1 == v2) && (l1 == l2) && (r1 == r2)
    _ == _ = False

-- 5
data AssocList k v = ALEmpty | ALCons k v (AssocList k v) deriving (Show)

instance Functor (AssocList k) where
    fmap _ ALEmpty = ALEmpty
    fmap f (ALCons k v rest) = ALCons k (f v) (fmap f rest)