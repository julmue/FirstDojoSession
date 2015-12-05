module Dojo.Data.Either
    ( Either (Left, Right)
    , either
    , lefts
    , rights
    , isLeft
    , isRight
    , partitionEithers
    ) where


data Either a b = Left a | Right b
    deriving (Show, Read, Eq, Ord)


either = undefined


lefts = undefined


rights = undefined


isLeft = undefined


isRight = undefined


partitionEithers = undefined
