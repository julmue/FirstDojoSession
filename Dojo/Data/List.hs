module Dojo.Data.List
   (
   -- * Basic functions
     (++)
   , head
   , last
   , tail
   , init
   , uncons
   , null
   , length

   -- * List transformations
   , map
   , reverse
   , intersperse
   , intercalate
   , transpose
   , subsequences
   , permutations

   -- * Reducing lists (folds)
   , foldl
   , foldl'
   , foldl1
   , foldl1'
   , foldr
   , foldr1

   ---- ** Special folds
   , concat
   , concatMap
   , and
   , or
   , any
   , all
   , sum
   , product
   , maximum
   , minimum

   -- * Building lists

   ---- ** Scans
   , scanl
   , scanl'
   , scanl1
   , scanr
   , scanr1

   ---- ** Accumulating maps
   , mapAccumL
   , mapAccumR

   ---- ** Infinite lists
   , iterate
   , repeat
   , replicate
   , cycle

   ---- ** Unfolding
   , unfoldr

   -- * Sublists
   ---- ** Extracting sublists
   , take
   , drop
   , splitAt
   , takeWhile
   , dropWhile
   , dropWhileEnd
   , span
   , break
   , stripPrefix
   , group
   , inits
   , tails

   ---- ** Predicates
   , isPrefixOf
   , isSuffixOf
   , isInfixOf
   , isSubsequenceOf

   -- * Searching lists
   ---- ** Searching by equality
   , elem
   , notElem
   , lookup

   ---- ** Searching with a predicate
   , find
   , filter
   , partition

   -- * Indexing lists
   -- | These functions treat a list @xs@ as a indexed collection,
   -- with indices ranging from 0 to @'length' xs - 1@.

   , (!!)

   , elemIndex
   , elemIndices

   , findIndex
   , findIndices

   -- * Zipping and unzipping lists

   , zip
   , zip3

   , unzip
   , unzip3
   -- * Special lists
   ---- ** Functions on strings
   , lines
   , words
   , unlines
   , unwords

   ---- ** \"Set\" operations
   , nub

   , delete
   , (\\)

   , union
   , intersect

   ---- ** Ordered lists
   , sort
   , sortOn
   , insert

   -- * Generalized functions

   ---- ** The \"@By@\" operations
   -- | By convention, overloaded functions have a non-overloaded
   -- counterpart whose name is suffixed with \`@By@\'.
   --
   -- It is often convenient to use these functions together with
   -- 'Data.Function.on', for instance @'sortBy' ('compare'
   -- \`on\` 'fst')@.

   ---- *** User-supplied equality (replacing an @Eq@ context)
   -- | The predicate is assumed to define an equivalence.
   , nubBy
   , deleteBy
   , deleteFirstsBy
   , unionBy
   , intersectBy
   , groupBy

   ---- *** User-supplied comparison (replacing an @Ord@ context)
   -- | The function is assumed to define a total ordering.
   , sortBy
   , insertBy
   , maximumBy
   , minimumBy

   ---- ** The \"@generic@\" operations
   -- | The prefix \`@generic@\' indicates an overloaded function that
   -- is a generalized version of a "Prelude" function.

   , genericLength
   , genericTake
   , genericDrop
   , genericSplitAt
   , genericIndex
   , genericReplicate

   ) where

(++) = undefined


head = undefined


last = undefined


tail = undefined


init = undefined


uncons = undefined


null = undefined


length = undefined

-- * List transformations
map = undefined


reverse = undefined


intersperse = undefined


intercalate = undefined


transpose = undefined


subsequences = undefined


permutations = undefined

-- ** Reducing lists (folds)
foldl = undefined


foldl' = undefined


foldl1 = undefined


foldl1' = undefined


foldr = undefined


foldr1 = undefined

-- *** Special folds
concat = undefined


concatMap = undefined


and = undefined


or = undefined


any = undefined


all = undefined


sum = undefined


product = undefined


maximum = undefined


minimum = undefined


-- * Building lists
---- ** Scans
scanl = undefined


scanl' = undefined


scanl1 = undefined


scanr = undefined


scanr1 = undefined


 ---- ** Accumulating maps
mapAccumL = undefined


mapAccumR = undefined


---- ** Infinite lists
iterate = undefined


repeat = undefined


replicate = undefined


cycle = undefined


-- ** Unfolding
unfoldr = undefined

-- * Sublists

-- ** Extracting sublists
take = undefined


drop = undefined


splitAt = undefined


takeWhile = undefined


dropWhile = undefined


dropWhileEnd = undefined


span = undefined


break = undefined


stripPrefix = undefined


group = undefined


inits = undefined


tails = undefined


-- ** Predicates
isPrefixOf = undefined


isSuffixOf = undefined


isInfixOf = undefined


isSubsequenceOf = undefined


-- * Searching lists
-- ** Searching by equality
elem = undefined


notElem = undefined


lookup = undefined

-- ** Searching with a predicate
find = undefined


filter = undefined


partition = undefined



-- * Indexing lists
(!!) = undefined

elemIndex = undefined


elemIndices = undefined


findIndex = undefined


findIndices = undefined


-- * Zipping and unzipping lists
zip = undefined


zip3 = undefined

unzip = undefined


unzip3 = undefined

-- * Special lists
-- ** Functions on strings
lines = undefined


words = undefined


unlines = undefined


unwords = undefined



-- ** "Set" operations
nub = undefined

delete = undefined


(\\) = undefined

union = undefined


intersect = undefined

-- ** Ordered lists
sort = undefined


sortOn = undefined


insert = undefined


-- * Generalized functions
-- *** User-supplied equality (replacing an @Eq@ context)
nubBy = undefined


deleteBy = undefined


deleteFirstsBy = undefined


unionBy = undefined


intersectBy = undefined


groupBy = undefined


-- *** User-supplied comparison (replacing an @Ord@ context)
sortBy = undefined


insertBy = undefined


maximumBy = undefined


minimumBy = undefined

-- ** The \"@generic@\" operations
genericLength = undefined


genericTake = undefined


genericDrop = undefined


genericSplitAt = undefined


genericIndex = undefined


genericReplicate = undefined
