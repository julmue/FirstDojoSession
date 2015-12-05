module Dojo.Data.Traversable
    ( Traversable (traverse, sequenceA, mapM, sequence)
    -- utility functions
    , for
    , forM
    , mapAccumL
    , mapAccumR
    -- general Definitions for Superclass methods
    , fmapDefault
    , foldMapDefault
    ) where


-- minimal complete definition: traverse, sequenceA
class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse = undefined
    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = undefined
    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    mapM = undefined
    sequence :: Monad m => t (m a) -> m (t a)
    sequence = undefined

-- utility functions
for = undefined


forM = undefined


mapAccumL = undefined


mapAccumR = undefined


-- general Definitions for Superclass methods
fmapDefault = undefined


foldMapDefault = undefined


