data ErrJst e j = Err e | Jst j deriving (Show)

instance Functor (ErrJst e) where
  fmap _ (Err e) = Err e
  fmap f (Jst j) = Jst (f j)

instance Applicative (ErrJst e) where
  pure = Jst
  Err e <*> _ = Err e
  _ <*> Err e = Err e
  Jst f <*> Jst x = Jst (f x)

instance Monad (ErrJst e) where
  return = Jst
  Err e >>= _ = Err e
  Jst x >>= f = f x

join :: Monad m => m (m a) -> m a
join mmx = mmx >>= id

data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)

instance Foldable LTree where
  foldr f acc (Leaf x) = f x acc
  foldr f acc (LNode left right) = foldr f (foldr f acc right) left


