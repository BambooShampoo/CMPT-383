data ErrJst e j = Err e | Jst j deriving (Show)

-- 1
instance Functor (ErrJst e) where
    fmap :: (a -> b) -> ErrJst e a -> ErrJst e b
    fmap _ (Err e) = Err e
    fmap f (Jst j) = Jst (f j)

-- 2
instance Applicative (ErrJst e) where
    pure :: a -> ErrJst e a
    pure = Jst

    (<*>) :: ErrJst e (a -> b) -> ErrJst e a -> ErrJst e b
    Err e <*> _ = Err e
    _ <*> Err e = Err e 
    Jst f <*> Jst x = Jst (f x)

-- 3
instance Monad (ErrJst e) where
    return :: a -> ErrJst e a
    return = pure

    (>>=) :: ErrJst e a -> (a -> ErrJst e b) -> ErrJst e b
    Err e >>= _ = Err e
    Jst x >>= f = f x

-- 4n 
join :: Monad m => m (m a) -> m a
join mmx = mmx >>= id

-- 5
data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)

instance Foldable LTree where
    foldr :: (a -> b -> b) -> b -> LTree a -> b
    foldr f acc (Leaf x) = f x acc 
    foldr f acc (LNode left right) = foldr f (foldr f acc right) left