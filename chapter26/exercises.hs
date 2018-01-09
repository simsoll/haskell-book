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
