import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

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
        (fab, s'') <- smfab s'
        return (fab a, s'')

-- 3.

instance Monad m => Monad (StateT s m) where
    return = pure

    StateT sma >>= fasmb = StateT $ \s -> do
        (a, s') <- sma s
        runStateT (fasmb a) s'

-- Wrap It Up

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT . ExceptT . ReaderT $ const (return (Right (Just 1)))

-- Lift More

class MonadTrans t where
    -- | Lift a computation from
    -- the argument monad to
    -- the constructed monad.
    lift :: (Monad m) => m a -> t m a

-- 1.

instance MonadTrans (EitherT e) where
    lift = EitherT . fmap Right


-- 2.

instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> do
        a <- ma
        return (a, s)

-- Some Instances


class (Monad m) => MonadIO m where
    -- | Lift a computation
    -- from the 'IO' monad.
    liftIO :: IO a -> m a

-- 1.
instance MonadTrans MaybeT where
    lift = MaybeT . fmap Just

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift .liftIO

-- 2.

instance MonadTrans (ReaderT r) where
    lift = ReaderT . const


instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift .liftIO

-- 3.

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift .liftIO
