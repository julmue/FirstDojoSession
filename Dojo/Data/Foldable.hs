module Dojo.Data.Foldable
    ( Foldable (fold, foldMap,
              foldr, foldr', foldl, foldl', foldr1, foldl1
              toList, null, length, elem, maximum, minimum,
              sum, product )
    -- Special biased folds
    , foldrM
    , foldlM
    -- applicative actions
    , traverse_
    , for_
    , sequenceA_
    , asum
    -- Monadic actions
    , mapM_
    , forM_
    , sequence_
    , msum
    -- Specialized folds
    , concat
    , concatMap
    , and
    , or
    , any
    , all
    , maximumBy
    , minimumBy
    -- Searches
    , notElem
    , find
    ) where

-- minimal complete definition: foldMap | foldr
class Foldable t where
    fold :: Monoid m => t m -> m
    fold = undefined
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap = undefined
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr = undefined
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldr' = undefined
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl = undefined
    foldl' :: (b -> a -> b) -> b -> t a -> b
    foldl' = undefined
    foldr1 :: (a -> a -> a) -> t a -> a
    foldr1 = undefined
    foldl1 :: (a -> a -> a) -> t a -> a
    foldl1 = undefined
    toList :: t a -> [a]
    toList = undefined
    null :: t a -> Bool
    null = undefined
    length :: t a -> Int
    length = undefined
    elem :: Eq a => a -> t a -> Bool
    elem = undefined
    maximum :: forall a . Ord a => t a -> a
    maximum = undefined
    minimum :: forall a . Ord a => t a -> a
    minimum = undefined
    sum :: forall n . Num n => t n -> n
    sum = undefined
    product :: forall n . Num n => t n -> n
    product = undefined




-- Special biased folds
foldrM = undefined


foldlM = undefined


-- applicative actions
traverse_ = undefined


for_ = undefined


sequenceA_ = undefined


asum = undefined


-- Monadic actions
mapM_ = undefined


forM_ = undefined


sequence_ = undefined


msum = undefined


-- Specialized folds
concat = undefined


concatMap = undefined


and = undefined


or = undefined


any = undefined


all = undefined


maximumBy = undefined


minimumBy = undefined


-- Searches
notElem = undefined


find = undefined
