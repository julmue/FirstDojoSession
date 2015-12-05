module Dojo.Control.Applicative
    ( Applicative (pure, (<*>),(*>),(<*))
    , Alternative (empty, (<|>), some, many))
    , ZipList (getZipList)
    , (<$>)
    , (<$)
    , (<**>)
    , liftA
    , liftA2
    , liftA3
    , optional
    ) where

-- minimal complete definition: pure, (<*>)
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    (*>) :: f a -> f b -> f b
    (*>) = undefined
    (<*) :: f a -> f b -> f a
    (<*) = undefined


-- minimal complete definition: empty, (<|>)
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    some :: f a -> f [a]
    some = undefined
    many :: f a -> f [a]
    many = undefined


-- utility functions
(<$>) = undefined

(<$) = undefined

(<**>) = undefined

liftA = undefined

liftA2 = undefined

liftA3 = undefined

optional = undefined

