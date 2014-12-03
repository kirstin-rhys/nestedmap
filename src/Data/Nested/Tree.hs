{-# LANGUAGE GADTs, NoImplicitPrelude, UnicodeSyntax #-}

module Data.Nested.Tree
       ( -- * Tree type
         Tree
         -- * Query
       , fruit, forest
       , null, size
       , lookup
         -- * Construction
       , empty
       , singleton
       , fromFoldable
       , fromList
         -- * List
       , toList
       ) where

import Data.Maybe (Maybe)
import Data.Ord (Ord)
import Data.Int (Int)
import Data.Bool (Bool)
import Data.Foldable (Foldable)

import Data.Nested.Internal ( Tree
                            , nullTree, fruit, forest
                            , sizeTree
                            , emptyTree
                            , singletonTree
                            , fromFoldableTree
                            , fromListTree
                            , toListTree
                            , lookupTree
                            )

empty ∷ α → Tree κ α
empty = emptyTree

null ∷ Tree κ α → Bool
null = nullTree

size ∷ Tree κ α → Int
size = sizeTree

lookup ∷ Ord κ ⇒ [κ] → Tree κ α → [Maybe α]
lookup = lookupTree

singleton ∷ Foldable φ ⇒ α → φ (κ,α) → Tree κ α
singleton = singletonTree

fromFoldable ∷ (Foldable φ, Foldable ψ, Ord κ) ⇒ α → ψ (φ (κ, α)) → Tree κ α
fromFoldable = fromFoldableTree

fromList ∷ (Ord κ) ⇒ α → [[(κ, α)]] → Tree κ α
fromList = fromListTree

toList ∷ Tree κ α → (α, [[(κ, α)]])
toList = toListTree
