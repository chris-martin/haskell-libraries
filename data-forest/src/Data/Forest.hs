{- |

Multi-way trees (also known as /rose trees/) and forests, similar to @Data.Tree@
from the popular /containers/ library.

-}

{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable,
             GeneralizedNewtypeDeriving #-}

module Data.Forest
    (
    -- * Importing
    -- $imports

    -- * Types
      Forest
    , Tree

    -- * Constructing
    , forest
    , tree
    , leaf
    , leaves

    -- * Deconstructing
    , trees
    , root
    , subforest
    , subtrees

    -- * Folds
    , foldForest
    , foldTree

    -- * Forest functor
    -- $functor

    ) where

import Data.Eq (Eq)
import Data.Foldable (Foldable)
import Data.Function (($))
import Data.Functor (Functor, (<$>), fmap)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup)
import Data.Traversable (Traversable)
import Prelude (Show)

--------------------------------------------------------------------------------

-- | A forest is defined completely by its 'trees'.
--
-- To construct a forest, use 'forest' or 'leaves'.

newtype Forest a = Forest
    { trees :: [Tree a] -- ^ The trees that constitute the forest.
    }
    deriving (Eq, Show, Functor, Foldable, Traversable, Semigroup, Monoid)

-- | A tree is defined completely by its 'root' and its 'subforest'.
--
-- To construct a tree, use 'tree' or 'leaf'.

data Tree a = Tree
    { root :: a             -- ^ The value at the root node of the tree.
    , subforest :: Forest a -- ^ The forest containing all descendants
                            --   of the tree's 'root'.
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

--------------------------------------------------------------------------------

-- | Construct a forest from a list of trees.
--
-- /@'forest' []@ is equivalent to 'mempty'./
forest :: [Tree a] -> Forest a
forest = Forest

-- | Construct a tree with a single root and no subforest.
--
--   /@'leaf' x@ is equivalent to @'tree' x 'mempty'@./
leaf :: a -> Tree a
leaf a = tree a mempty

-- | Construct a forest of depth 1, where each tree contains only a root.
--
-- /'leaves' is equivalent to @'forest' . fmap 'leaf'@/
leaves :: [a] -> Forest a
leaves xs =
  forest (fmap leaf xs)

-- | Construct a tree with a root and subforest.
tree :: a -> Forest a -> Tree a
tree = Tree

-- | The tree's immediate subtrees.
--
-- /'subtrees' is equivalent to @'trees' . 'subforest'@./
subtrees :: Tree a -> [Tree a]
subtrees t = trees (subforest t)

{- | Catamorphism on forests.

>>>
:{
example :: Forest Char
example = forest
    [ tree 'a' $ leaves "bc"
    , tree 'd' $ forest
        [ leaf 'e'
        , tree 'f' $ leaves "g"
        ]
   ]
:}

>>> foldForest (intercalate ", " . fmap (\(a, b) -> [a] <> " [" <> b <> "]")) example
"a [b [], c []], d [e [], f [g []]]"

-}
foldForest :: ([(a, b)] -> b) -> Forest a -> b
foldForest f =
    go
  where
    go (Forest ts) = f $ (\t -> (root t, go (subforest t))) <$> ts

{- | Catamorphism on trees.

>>>
:{
example :: Tree Char
example = tree 'a' $ forest
    [ tree 'b' $ leaves "cd"
    , tree 'e' $ forest
        [ leaf 'f'
        , tree 'g' $ leaves "h"
        ]
   ]
:}

>>> foldTree (\a bs -> [a] <> " [" <> intercalate ", " bs <> "]") example
"a [b [c [], d []], e [f [], g [h []]]]"

-}
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f =
    go
  where
    go t = f (root t) (go <$> subtrees t)


--------------------------------------------------------------------------------

{- $setup

>>> import Prelude
>>> import Data.Char
>>> import Data.Foldable
>>> import Data.Function
>>> import Data.List
>>> import Data.Semigroup

-}

--------------------------------------------------------------------------------

{- $imports

Recommended imports:

> import Data.Forest (Forest, Tree)
> import qualified Data.Forest as Forest

-}

--------------------------------------------------------------------------------

{- $functor

One notable difference of this 'Forest' from that of the /containers/ library is
that this 'Forest' is a newtype rather than a type alias, and so it provides a
more appropriate 'Functor' instance:

>>>
:{
example :: Forest Char
example = forest
    [ tree 'a' $ leaves "bc"
    , tree 'd' $ forest
        [ leaf 'e'
        , tree 'f' $ leaves "g"
        ]
   ]
:}

>>>
:{
showCharForest f =
    intercalate ", " (showCharTree <$> trees f)
  where
    showCharTree t = case trees (subforest t) of
      []   -> [root t]
      [t'] -> [root t] <> ": " <> showCharTree t'
      ts   -> [root t] <> ": (" <> showCharForest (subforest t) <> ")"
:}

>>> showCharForest example
"a: (b, c), d: (e, f: g)"

>>> showCharForest (fmap toUpper example)
"A: (B, C), D: (E, F: G)"

Likewise, 'Forest''s 'Foldable' instance folds over the elements of the forest.

>>> toList example
"abcdefg"

-}
