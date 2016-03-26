{-# LANGUAGE GADTs, NoImplicitPrelude, UnicodeSyntax #-}

module Data.Nested.Tree
       ( -- * Tree type
         Tree
         -- * Query
       , fruit, forest
       , null, size
       , lookup, member
         -- * Construction
       , empty
       , singleton
       , fromFoldable
       , fromList
         -- * List
       , toList
         -- * Semigroupy
       , union
       , unionWithKey
       , unionWithKey'
       , unionWith
         -- * Foldy
       , foldrWithAncestors1
       , foldrWithAncestorsAndLeafMarker1
       ) where


import Data.Function (flip)
import Data.Maybe (Maybe)
import Data.Ord (Ord)
import Data.Int (Int)
import Data.Bool (Bool)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Data.Nested.Internal ( Tree
                            , nullTree, fruit, forest
                            , sizeTree
                            , emptyTree
                            , singletonTree
                            , fromFoldableTree
                            , fromListTree
                            , toListTree
                            , lookupTree
                            , memberTree
                            , unionTree
                            , unionTreeWithKey
                            , unionTreeWithKey'
                            , unionTreeWith
                            , foldrTreeWithAncestors1
                            , foldrTreeWithAncestorsAndLeafMarker1
                            )

empty ∷ α → Tree κ α
empty = emptyTree

null ∷ Tree κ α → Bool
null = nullTree

size ∷ Tree κ α → Int
size = sizeTree

lookup ∷ (Traversable φ, Ord κ) ⇒ φ κ → Tree κ α → (α, φ (Maybe α))
lookup = flip lookupTree

member ∷ (Traversable φ, Ord κ) ⇒ φ κ → Tree κ α → φ Bool
member = flip memberTree

singleton ∷ Foldable φ ⇒ α → φ (κ,α) → Tree κ α
singleton = singletonTree

fromFoldable ∷ (Foldable φ, Foldable ψ, Ord κ) ⇒ α → ψ (φ (κ, α)) → Tree κ α
fromFoldable = fromFoldableTree

fromList ∷ (Ord κ) ⇒ α → [[(κ, α)]] → Tree κ α
fromList = fromListTree

toList ∷ Tree κ α → (α, [[(κ, α)]])
toList = toListTree

union ∷ Ord κ ⇒ Tree κ α → Tree κ α → Tree κ α
union = unionTree 

unionWithKey ∷  Ord κ ⇒ (α → α → α) → (κ → α → α → α) → Tree κ α → Tree κ α → Tree κ α
unionWithKey = unionTreeWithKey 

unionWithKey' ∷ Ord κ ⇒ (κ → α → α → α) → κ → Tree κ α → Tree κ α → Tree κ α
unionWithKey' = unionTreeWithKey'

unionWith ∷ Ord κ ⇒ (α → α → α) → Tree κ α → Tree κ α → Tree κ α
unionWith = unionTreeWith

foldrWithAncestors1 ∷ ([(κ, α)] → β → β) → [(κ, α)] → κ → Tree κ α → β → β
foldrWithAncestors1 = foldrTreeWithAncestors1

foldrWithAncestorsAndLeafMarker1 ∷ (Bool → [(κ, α)] → β → β) → [(κ, α)] → κ → Tree κ α → β → β
foldrWithAncestorsAndLeafMarker1 = foldrTreeWithAncestorsAndLeafMarker1


