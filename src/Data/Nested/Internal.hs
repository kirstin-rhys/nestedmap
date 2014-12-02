{-# LANGUAGE GADTs, NoImplicitPrelude, UnicodeSyntax #-}

module Data.Nested.Internal
       ( -- * Tree and Forest types
         Tree, Forest
         -- * Query
       , fruit, forest, trees, treeAssocs
       , nullTree, nullForest
       , sizeTree, sizeForest
         -- * Construction
       , emptyTree, emptyForest
       , singletonTree, singletonForest
       , fromFoldableTree, fromFoldableForest
       , fromListTree, fromListForest
         -- * List
       , toListForest, toListTree
       ) where

import qualified Data.List as L
import Prelude.Unicode ((⊥))
import Prelude (Num, (+))
import Data.Int (Int)
import Data.Bool (Bool, otherwise)
import Data.Ord (Ord)
import Data.Tuple (uncurry)
import Data.Function (flip, ($), const, id)
import Data.Function.Unicode ((∘))
import Data.Functor (Functor, fmap)
import Data.Foldable (Foldable, foldr, foldMap)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Data.Monoid.Unicode ((⊕))
import Text.Show (Show)
import Data.Map (Map)
import qualified Data.Map as M

data Tree κ α where
  Tree ∷ { fruit  ∷ ! α
         , forest ∷ Forest κ α
         } → Tree κ α
  deriving (Show)

data Forest κ α where
  Forest ∷ { unForest ∷ Map κ (Tree κ α) } → Forest κ α
  deriving (Show)

instance Functor (Forest κ) where
  fmap = mapForest

instance Functor (Tree κ) where
  fmap = mapTree

instance (Ord κ, Monoid α) ⇒ Monoid (Forest κ α) where
  mempty  = emptyForest
  mappend = unionForestWith (⊕)

instance (Ord κ, Monoid α) ⇒ Monoid (Tree κ α) where
  mempty          = Tree mempty mempty
  t1 `mappend` t2 = Tree (fruit t1 ⊕ fruit t2) (forest t1 ⊕ forest t2)

instance Foldable (Forest κ) where
  foldMap f = foldMap (foldMap f) ∘ unForest
  foldr     = foldrForest

instance Foldable (Tree κ) where
  foldMap g (Tree x f) = (g x) ⊕ (foldMap g f)
  foldr                = foldrTree

nullForest ∷ Forest κ α → Bool
nullForest = M.null ∘ unForest

nullTree ∷ Tree κ α → Bool
nullTree = nullForest ∘ forest

trees ∷ Forest κ α → [Tree κ α]
trees = M.elems ∘  unForest

treeAssocs ∷ Forest κ α → [(κ, Tree κ α)]
treeAssocs = M.assocs ∘ unForest

sizeForest ∷ Forest κ α → Int
sizeForest = foldr (const (+1)) 0

sizeTree ∷ Tree κ α → Int
sizeTree = (+1) ∘ sizeForest ∘ forest

emptyForest ∷ Forest κ α
emptyForest = Forest M.empty

emptyTree ∷ α → Tree κ α
emptyTree v = Tree v emptyForest

singletonForest ∷ Foldable φ ⇒ φ (κ,α) → Forest κ α
singletonForest = foldr (uncurry singleton) emptyForest
  where singleton k v = Forest ∘ M.singleton k ∘ Tree v

singletonTree ∷ Foldable φ ⇒ α → φ (κ,α) → Tree κ α
singletonTree x = Tree x ∘ singletonForest

fromFoldableForest ∷ (Foldable φ, Foldable ψ, Ord κ) ⇒ ψ (φ (κ, α)) → Forest κ α
fromFoldableForest = foldr (unionForest ∘ singletonForest)  emptyForest

fromFoldableTree ∷ (Foldable φ, Foldable ψ, Ord κ) ⇒ α → ψ (φ (κ, α)) → Tree κ α
fromFoldableTree x = Tree x ∘ fromFoldableForest

fromListForest ∷ Ord κ ⇒ [[(κ, α)]] → Forest κ α
fromListForest = fromFoldableForest

fromListTree ∷ Ord κ ⇒ α → [[(κ, α)]] → Tree κ α
fromListTree = fromFoldableTree

toListForest ∷ Forest κ α → [[(κ, α)]]
toListForest = fmap L.reverse ∘ foldrForestWithAncestorsAndLeafMarker leafCons []
  where leafCons b = if b then (:) else flip const

toListTree ∷ Tree κ α → (α, [[(κ, α)]])
toListTree t = (fruit t, toListForest (forest t))


mapForest ∷ (α → β) → Forest κ α → Forest κ β
mapForest f = Forest ∘ M.map (mapTree f) ∘ unForest

mapTree ∷ (α → β) → Tree κ α → Tree κ β
mapTree f (Tree v ts) = Tree (f v) (mapForest f ts)

foldrForest ∷ (α → β → β) → β → Forest κ α → β
foldrForest f z = M.foldr (flip $ foldrTree f) z ∘ unForest

foldrTree ∷ (α → β → β) → β → Tree κ α → β
foldrTree f z (Tree x fst) = f x (foldrForest f z fst)

unionForest ∷ Ord κ ⇒ Forest κ α → Forest κ α → Forest κ α
unionForest (Forest f1) (Forest f2) = Forest $ M.unionWith unionTree f1 f2

unionTree ∷ Ord κ ⇒ Tree κ α → Tree κ α → Tree κ α
unionTree (Tree x1 f1) (Tree x2 f2) = Tree x2 (unionForest f1 f2)


unionForestWithKey ∷ Ord κ ⇒ (κ → α → α → α) → Forest κ α → Forest κ α → Forest κ α
unionForestWithKey f (Forest m1) (Forest m2) = Forest $ M.unionWithKey (unionTreeWithKey' f) m1 m2

unionForestWith ∷ Ord κ ⇒ (α → α → α) → Forest κ α → Forest κ α → Forest κ α
unionForestWith f = unionForestWithKey (const f)

unionTreeWithKey' ∷ Ord κ ⇒ (κ → α → α → α) → κ → Tree κ α → Tree κ α → Tree κ α
unionTreeWithKey' f k t1 t2 = Tree (f k (fruit t1) (fruit t2)) (unionForestWithKey f (forest t1) (forest t2))

unionTreeWithKey ∷ Ord κ ⇒ (α → α → α) → (κ → α → α → α) → Tree κ α → Tree κ α → Tree κ α
unionTreeWithKey g f t1 t2 = Tree (g (fruit t1) (fruit t2)) (unionForestWithKey f (forest t1) (forest t2))

unionTreeWith ∷ Ord κ ⇒ (α → α → α) → Tree κ α → Tree κ α → Tree κ α
unionTreeWith f = unionTreeWithKey f (const f)




foldrForestWithAncestors ∷ ([(κ, α)] → β → β) → β → Forest κ α → β
foldrForestWithAncestors f = foldrForestWithAncestors1 f []

foldrForestWithAncestors1 ∷ ([(κ, α)] → β → β) → [(κ, α)] → β → Forest κ α → β
foldrForestWithAncestors1 f kvs z = M.foldrWithKey (foldrTreeWithAncestors1 f kvs) z ∘ unForest

foldrTreeWithAncestors1 ∷ ([(κ, α)] → β → β) → [(κ, α)] → κ → Tree κ α → β → β
foldrTreeWithAncestors1 f kvs k t z = f as (foldrForestWithAncestors1 f as z (forest t))
  where as = (k, fruit t):kvs



foldrForestWithAncestorsAndLeafMarker ∷ (Bool → [(κ, α)] → β → β) → β → Forest κ α → β
foldrForestWithAncestorsAndLeafMarker f = foldrForestWithAncestorsAndLeafMarker1 f []

foldrForestWithAncestorsAndLeafMarker1 ∷ (Bool → [(κ, α)] → β → β) → [(κ, α)] → β → Forest κ α → β
foldrForestWithAncestorsAndLeafMarker1 f kvs z = M.foldrWithKey (foldrTreeWithAncestorsAndLeafMarker1 f kvs) z ∘ unForest

foldrTreeWithAncestorsAndLeafMarker1 ∷ (Bool → [(κ, α)] → β → β) → [(κ, α)] → κ → Tree κ α → β → β
foldrTreeWithAncestorsAndLeafMarker1 f kvs k t z = f isLeaf as (foldrForestWithAncestorsAndLeafMarker1 f as z (forest t))
  where as = (k, fruit t):kvs
        isLeaf = nullTree t
