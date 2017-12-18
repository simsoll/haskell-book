 -- Traversable instances

 -- Identity

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

-- Constant

newtype Constant a b =
    Constant { getConstant :: a}
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure $ Constant a

-- Maybe

data Optional a =
    Nada
    | Yep a
    deriving (Eq, Ord, Show)

instance Functor Optional where
    fmap _ Nada    = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldMap _ Nada    = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a

-- List

data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Ord, Show)

instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Foldable List where
    foldMap _ Nil         = mempty
    foldMap f (Cons a as) = mappend (f a) (foldMap f as)

instance Traversable List where
    traverse _ Nil         = pure Nil
    traverse f (Cons a as) = Cons <$> f a <*> traverse f as

-- Three

data Three a b c =
    Three a b c
    deriving (Eq, Ord, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

-- Pair

data Pair a b =
    Pair a b
    deriving (Eq, Ord, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
    foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b

-- Big

data Big a b =
    Big a b b
    deriving (Eq, Ord, Show)

instance Functor (Big a) where
    fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
    foldMap f (Big a b1 b2) = mappend (f b1) (f b2)

instance Traversable (Big a) where
    traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2

-- Bigger

data Bigger a b =
    Bigger a b b b
    deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
    foldMap f (Bigger a b1 b2 b3) = mappend (mappend (f b1) (f b2)) (f b3)

instance Traversable (Bigger a) where
    traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3

-- S

data S n a =
    S (n a) a
    deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S n a) = mappend (foldMap f n) (f a)

instance Traversable n => Traversable (S n) where
    traverse f (S n a) = S <$> traverse f n <*> f a

-- Tree

data Tree a =
    Empty
    | Leaf a
    | Node (Tree a ) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty         = Empty
    fmap f (Leaf a)      = Leaf $ f a
    fmap f (Node t a t') = Node (fmap f t) (f a) (fmap f t')

instance Foldable Tree where
    foldMap _ Empty    = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node t a t') = (foldMap f t) `mappend` (f a) `mappend` (foldMap f t')

instance Traversable Tree where
    traverse _ Empty         = pure Empty
    traverse f (Leaf a)      = Leaf <$> f a
    traverse f (Node t a t') = Node <$> traverse f t <*> f a <*> traverse f t'
