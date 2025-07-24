{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}

module Data.Nested.Strict.Internal
       ( -- * Tree and Forest types
         Tree, Forest
         -- * Query
       , fruit, forest, trees, treeAssocs
       , nullTree, nullForest
       , sizeTree, sizeForest
       , lookupTree, lookupForest
       , memberTree, memberForest
         -- * Construction
       , emptyTree, emptyTree', emptyForest
       , singletonTree, singletonForest
       , fromFoldableTree, fromFoldableForest
         -- * List
       , toListForest, toListTree
       , fromListTree, fromListForest
         -- * Semigroupy
       , unionTree, unionForest
       , unionTreeWithKey, unionForestWithKey
       , unionTreeWithKey'
       , unionTreeWith, unionForestWith
         -- * Foldablish
       , foldrForestWithAncestors
       , foldrForestWithAncestors1
       , foldrTreeWithAncestors1
       , foldrForestWithAncestorsAndLeafMarker
       , foldrForestWithAncestorsAndLeafMarker1
       , foldrTreeWithAncestorsAndLeafMarker1
       ) where

import Data.List qualified as L
import Prelude.Unicode ((⊥))
import Prelude (Num, (+), (&&), error)
import Data.Eq (Eq(..))
import Data.Maybe (Maybe(Just, Nothing), maybe, isJust)
import Data.Int (Int)
import Data.Bool (Bool, otherwise)
import Data.Ord (Ord(..), Ordering(..))
import Data.Tuple (uncurry, snd)
import Data.Function (flip, ($), const, id)
import Data.Function.Unicode ((∘))
import Data.Functor (Functor, fmap, (<$>))
import Data.Foldable (Foldable, foldr, foldMap)
import Data.Traversable (Traversable, mapAccumL, traverse)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Data.Monoid.Unicode ((⊕))
import Text.Show (Show, showsPrec)
import Control.Arrow ((&&&))
import Control.Monad (MonadPlus, (>>=), join, return, mplus)
import Control.Applicative (Applicative)
import Control.Applicative.Unicode ((⊛))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

#if __GLASGOW_HASKELL__
import Data.Data (Data(..), mkNoRepType, gcast2)
#endif




data Tree κ α where
  Tree ∷ { fruit  ∷ !α
         , forest ∷ !(Forest κ α)
         } → Tree κ α
  deriving (Show)

newtype Forest κ α where
  Forest ∷ { unForest ∷ Map κ (Tree κ α) } → Forest κ α
  deriving (Show)

instance Functor (Forest κ) where
  fmap f = Forest ∘ ((f <$>) <$>) ∘ unForest

instance Functor (Tree κ) where
  fmap f (Tree v ts) = Tree (f v) (f <$> ts)

instance (Ord κ, Semigroup α) ⇒ Semigroup (Forest κ α) where
  (<>) = unionForestWith (<>)

instance (Ord κ, Monoid α) ⇒ Monoid (Forest κ α) where
  mempty  = emptyForest
  mappend = (<>)

instance (Ord κ, Semigroup α) ⇒ Semigroup (Tree κ α) where
  t1 <> t2 = Tree (fruit t1 <> fruit t2) (forest t1 <> forest t2)

instance (Ord κ, Monoid α) ⇒ Monoid (Tree κ α) where
  mempty  = Tree mempty mempty
  mappend = (<>)

instance Foldable (Forest κ) where
  foldMap f = foldMap (foldMap f) ∘ unForest
  foldr f z = foldr (flip $ foldr f) z ∘ unForest

instance Foldable (Tree κ) where
  foldMap f             = (f ∘ fruit) ⊕ (foldMap f ∘ forest)
  foldr f z (Tree v ts) = f v (foldr f z ts)

instance Traversable (Forest κ) where
  traverse f = (Forest <$>) <$> traverse (traverse f) ∘ unForest

instance Traversable (Tree κ) where
  traverse f (Tree v ts) = Tree <$> f v ⊛ traverse f ts

instance (Eq κ, Eq α) ⇒ Eq (Forest κ α) where
  (Forest m1) == (Forest m2) = m1 == m2

instance (Eq κ, Eq α) ⇒ Eq (Tree κ α) where
  (Tree v1 f1) == (Tree v2 f2) = v1 == v2 && f1 == f2

instance (Ord κ, Ord α) ⇒ Ord (Forest κ α) where
  compare f1 f2 = unForest f1 `compare` unForest f2

instance (Ord κ, Ord α) ⇒ Ord (Tree κ α) where
  compare t1 t2 = case fruit t1 `compare` fruit t2 of
    EQ  -> forest t1 `compare` forest t2
    cmp -> cmp

#if __GLASGOW_HASKELL__

instance (Data κ, Data α, Ord κ) ⇒ Data (Forest κ α) where
  gfoldl f z fs = z fromListForest `f` toListForest fs
  toConstr _    = error "toConstr"
  gunfold _ _   = error "gunfold"
  dataTypeOf _  = mkNoRepType "Data.Nested.Internal.Forest"
  dataCast2 f   = gcast2 f

instance (Data κ, Data α, Ord κ) ⇒ Data (Tree κ α) where
  gfoldl f z t = z (fromListTree v) `f` xss where (v, xss) = toListTree t
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Nested.Internal.Tree"
  dataCast2 f  = gcast2 f

#endif

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

-- a more general version would use Folable φ as input and a user-specifiable Monoid output
lookupForest ∷ (Traversable φ, Ord κ) ⇒ Forest κ α → φ κ → φ (Maybe α)
lookupForest f = snd ∘ mapAccumL (flip lookup) (Just f)
  where lookup ∷ Ord κ ⇒ κ → Maybe (Forest κ α) → (Maybe (Forest κ α), Maybe α)
        lookup k = (fmap forest &&& fmap fruit) ∘ join ∘ fmap (M.lookup k ∘ unForest)

lookupTree ∷ (Traversable φ, Ord κ) ⇒ Tree κ α → φ κ → (α, φ (Maybe α))
lookupTree t = (fruit t,) ∘ lookupForest (forest t)

memberTree ∷ (Traversable φ, Ord κ) ⇒ Tree κ α → φ κ → φ Bool
memberTree t = (isJust <$>) ∘ snd ∘ lookupTree t

memberForest ∷ (Traversable φ, Ord κ) ⇒ Forest κ α → φ κ → φ Bool
memberForest f = (isJust <$>) ∘ lookupForest f

emptyForest ∷ Forest κ α
emptyForest = Forest M.empty

emptyTree ∷ Monoid α ⇒ Tree κ α
emptyTree = emptyTree' mempty

emptyTree' ∷ α → Tree κ α
emptyTree' v = Tree v emptyForest

singletonForest ∷ Foldable φ ⇒ φ (κ,α) → Forest κ α
singletonForest = foldr (uncurry singleton) emptyForest
  where singleton k v = Forest ∘ M.singleton k ∘ Tree v

singletonTree ∷ Foldable φ ⇒ α → φ (κ,α) → Tree κ α
singletonTree v = Tree v ∘ singletonForest

fromFoldableForest ∷ (Foldable φ, Foldable ψ, Ord κ) ⇒ ψ (φ (κ, α)) → Forest κ α
fromFoldableForest = foldr (unionForest ∘ singletonForest)  emptyForest

fromFoldableTree ∷ (Foldable φ, Foldable ψ, Ord κ) ⇒ α → ψ (φ (κ, α)) → Tree κ α
fromFoldableTree v = Tree v ∘ fromFoldableForest

fromListForest ∷ Ord κ ⇒ [[(κ, α)]] → Forest κ α
fromListForest = fromFoldableForest

fromListTree ∷ Ord κ ⇒ α → [[(κ, α)]] → Tree κ α
fromListTree = fromFoldableTree

toListForest ∷ Forest κ α → [[(κ, α)]]
toListForest = fmap L.reverse ∘ foldrForestWithAncestorsAndLeafMarker leafCons []
  where leafCons b = if b then (:) else flip const

toListTree ∷ Tree κ α → (α, [[(κ, α)]])
toListTree t = (fruit t, toListForest (forest t))

unionForest ∷ Ord κ ⇒ Forest κ α → Forest κ α → Forest κ α
unionForest (Forest f1) (Forest f2) = Forest $ M.unionWith unionTree f1 f2

unionTree ∷ Ord κ ⇒ Tree κ α → Tree κ α → Tree κ α
unionTree (Tree _x1 f1) (Tree x2 f2) = Tree x2 (unionForest f1 f2)

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
