module Dojo.Control.Monad
    ( Functor(fmap)
    , Monad((>>=), (>>), return)
    , MonadPlus(mzero, mplus)
    , mapM
    , mapM_
    , forM
    , forM_
    , sequence
    , sequence_
    , (=<<)
    , (>=>)
    , (<=<)
    , forever
    , void
    , join
    , msum
    , filterM
    , mapAndUnzipM
    , zipWithM
    , zipWithM_
    , foldM
    , foldM_
    , replicateM
    , replicateM_
    , guard
    , when
    , unless
    , liftM
    , liftM2
    , liftM3
    , liftM5
    , ap
    ) where

import Prelude(undefined)

-- minimal definition: return, (>>=)
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b


-- minimal complete definition: nothing
class (Alternative m, Monad m) => MonadPlus m where
    mzero :: m a
    mzero = undefined
    mplus :: m a -> m a -> m a
    mplus = undefined


mapM = undefined


mapM_ = undefined


forM = undefined


forM_ = undefined


sequence = undefined


sequence_ = undefined


(=<<) = undefined


(>=>) = undefined


(<=<) = undefined


forever = undefined


void = undefined


join = undefined


msum = undefined


filterM = undefined


mapAndUnzipM = undefined


zipWithM = undefined


zipWithM_ = undefined


foldM = undefined


foldM_ = undefined


replicateM = undefined


replicateM_ = undefined


guard = undefined


when = undefined


unless = undefined


liftM = undefined


liftM2 = undefined


liftM3 = undefined


liftM5 = undefined


ap = undefined
