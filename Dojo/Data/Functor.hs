module Dojo.Data.Functor
    ( Functor(fmap)
    , (<$)
    , (<$>)
    , ($>)
    , void
    ) where

class Functor f where
    fmap :: (a -> b) -> f a -> f b


(<$) = undefined


(<$>) = undefined


($>) = undefined


void = undefined
