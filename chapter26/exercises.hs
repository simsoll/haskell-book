-- EitherT

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

-- 1.

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

-- 2.

instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT (pure (pure x))

    (EitherT fab) <*> (EitherT mma) = EitherT $ (<*>) <$> fab <*> mma

-- 3.

instance Monad m => Monad (EitherT e m) where
    return = pure

    EitherT ma >>= f =
        EitherT $ do
            v <- ma
            case v of
                Left e  -> return $ Left e
                Right a -> runEitherT (f a)

-- 4.

swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

-- 5.

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT amb) = do
    e <- amb
    case e of
        Left a  -> fa a
        Right b -> fb b


-- StateT

newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }

-- 1.

instance Functor m => Functor (StateT s m) where
    fmap f (StateT sma) = StateT $ (fmap . fmap) (\(a, s) -> (f a, s)) sma

-- 2.

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> return (a, s)
    StateT smfab <*> StateT sma = StateT $ \s -> do
        (a, s') <- sma s
        (fab, s'') <- smfab s
        return (fab a, s'')
