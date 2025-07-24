{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.Nested.Strict.Forest
       ( -- * Forest type
         Forest
         -- * Query
       , trees, treeAssocs
       , null, size
       , lookup, member
         -- * Construction
       , empty
       , singleton
       , fromFoldable
       , fromList
         -- * List
       , toList
         -- * Semigroupish
       , union
       , unionWithKey
       , unionWith
         -- * Foldable Plus
       , foldrWithAncestors
       , foldrWithAncestors1
       , foldrWithAncestorsAndLeafMarker
       , foldrWithAncestorsAndLeafMarker1
       ) where

import Data.Function (flip)
import Data.Maybe (Maybe)
import Data.Ord (Ord)
import Data.Int (Int)
import Data.Bool (Bool)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
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
                            , memberForest
                            , unionForest
                            , unionForestWithKey
                            , unionForestWith
                            , foldrForestWithAncestors
                            , foldrForestWithAncestors1
                            , foldrForestWithAncestorsAndLeafMarker
                            , foldrForestWithAncestorsAndLeafMarker1
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

lookup ∷ (Traversable φ, Ord κ) ⇒ φ κ → Forest κ α → φ (Maybe α)
lookup = flip lookupForest

member ∷ (Traversable φ, Ord κ) ⇒ φ κ → Forest κ α → φ Bool
member = flip memberForest

union ∷ Ord κ ⇒ Forest κ α → Forest κ α → Forest κ α
union = unionForest

unionWithKey ∷ Ord κ ⇒ (κ → α → α → α) → Forest κ α → Forest κ α → Forest κ α
unionWithKey = unionForestWithKey

unionWith ∷ Ord κ ⇒ (α → α → α) → Forest κ α → Forest κ α → Forest κ α
unionWith = unionForestWith

foldrWithAncestors ∷ ([(κ, α)] → β → β) → β → Forest κ α → β
foldrWithAncestors = foldrForestWithAncestors

foldrWithAncestors1 ∷ ([(κ, α)] → β → β) → [(κ, α)] → β → Forest κ α → β
foldrWithAncestors1  = foldrForestWithAncestors1

foldrWithAncestorsAndLeafMarker ∷ (Bool → [(κ, α)] → β → β) → β → Forest κ α → β
foldrWithAncestorsAndLeafMarker = foldrForestWithAncestorsAndLeafMarker

foldrWithAncestorsAndLeafMarker1 ∷ (Bool → [(κ, α)] → β → β) → [(κ, α)] → β → Forest κ α → β
foldrWithAncestorsAndLeafMarker1 = foldrForestWithAncestorsAndLeafMarker1
