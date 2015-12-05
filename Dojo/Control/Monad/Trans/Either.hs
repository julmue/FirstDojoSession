-- from E.Kmetts either package
-- https://hackage.haskell.org/package/either-4.4.1/docs/src/Control-Monad-Trans-Either.html

Module Dojo.Control.Monad.Trans.Either
    ( EitherT(..)
    , eitherT
    , bimapEitherT
    , mapEitherT
    , hoistEitherT
    , bracketEitherT
    , bracketEitherT_
    , left
    , right
    , swapEitherT
    ) where

newtype EitherT = EitherT ()

-- instances
-- Show
-- Read
-- Ord
-- Eq
-- Functor
-- Applicative
-- Alternative
-- Monad
-- MonadPlus
-- a lot of other ... do look up ...
