{-# LANGUAGE GADTs, NoImplicitPrelude, UnicodeSyntax #-}

module Data.Nested.Forest
       ( -- * Forest type
         Forest
         -- * Query
       , trees, treeAssocs
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
import Data.Nested.Internal ( Forest
                            , trees, treeAssocs
                            , nullForest
                            , sizeForest
                            , emptyForest
                            , singletonForest
                            , fromFoldableForest
                            , fromListForest
                            , toListForest
                            , lookupForest
                            )

empty ∷ Forest κ α
empty = emptyForest

null ∷ Forest κ α → Bool
null = nullForest

size ∷ Forest κ α → Int
size = sizeForest

singleton ∷ Foldable φ ⇒ φ (κ,α) → Forest κ α
singleton = singletonForest

fromFoldable ∷ (Foldable φ, Foldable ψ, Ord κ) ⇒ ψ (φ (κ, α)) → Forest κ α
fromFoldable = fromFoldableForest

fromList ∷ (Ord κ) ⇒ [[(κ, α)]] → Forest κ α
fromList = fromListForest

toList ∷ Forest κ α → [[(κ, α)]]
toList = toListForest

lookup ∷ Ord κ ⇒ [κ] → Forest κ α → [Maybe α]
lookup = lookupForest
