{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dojo.Control.Monad.Trans.Identity
    ( IdentityT (runIdentityT)
    , mapIdentityT
    -- lifting operations
    , liftCatch
    , liftCallCC
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

-- the trivial monad transformer which maps a monad to an equivalent monad.
-- IdentityT :: (* -> *) -> * -> *
newtype IdentityT m a = IdentityT { runIdentityT :: m a }
    deriving(Read, Show, Eq, Ord)

instance MonadTrans IdentityT where
    -- lift :: m a -> t m a
    lift = IdentityT

instance (Functor m) => Functor (IdentityT m) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f = IdentityT . fmap f . runIdentityT

instance (Functor m, Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)
    (<*>) = lift2IdentityT (<*>)

instance (Alternative m) => Alternative (IdentityT m) where
    empty = IdentityT empty
    (<|>) = lift2IdentityT (<|>)

instance (Applicative m, Monad m) => Monad (IdentityT m) where
    return = pure
    -- >>= :: IdentityT m a -> (a -> Identity m b) -> Identity m b)
    ia >>= f = IdentityT $ (\a -> runIdentityT $ f a) =<< runIdentityT ia

--  ia >>= f = IdentityT $ do
--      a <- runIdentityT a
--      runIdentityT $ f a

instance (Applicative m, MonadPlus m) => MonadPlus (IdentityT m) where
    mzero = lift mzero
    mplus = lift2IdentityT mplus


-- important helper function:
-- lift a binary operation to the new monad
lift2IdentityT :: (m a -> n b -> p c) -> IdentityT m a -> IdentityT n b -> IdentityT p c
lift2IdentityT f a b = IdentityT $ f (runIdentityT a) (runIdentityT b)

-- instance MonadFix
-- instance Foldable
-- instance Traversable
-- instance MonadPlus
-- instance MonadIO

-- lift an unary operation to the new monad
mapIdentityT :: (m a -> n b) -> IdentityT m a -> IdentityT n b
mapIdentityT f = IdentityT . f . runIdentityT


liftCatch = undefined


liftCallCC = undefined

