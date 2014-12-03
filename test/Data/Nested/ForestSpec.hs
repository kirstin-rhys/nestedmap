{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Data.Nested.ForestSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers

import qualified Data.List as L
import qualified Data.List.Ordered as OL
import Data.Function.Unicode ((∘))
import Prelude.Unicode ((⊥))
import Data.Int (Int)
import Data.Char (Char)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Bool (not)
import Data.Bool.Unicode ((∨))
import Data.Eq.Unicode ((≡))
import Data.Function (($))
import Data.Foldable (all)
import Data.Tuple (uncurry, fst, snd)
import Data.Functor (fmap)
import Data.Bool (Bool(..))
import Data.Nested.Forest ( empty
                          , null
                          , size
                          , singleton
                          , fromList
                          , toList
                          , lookup
                          )

spec :: Spec
spec = describe "Forest" $ do
  prop "null empty should always be true"                                    prop_null_empty
  prop "the size of a singleton forest should be the length of the argument" prop_singleton_size
  prop "a unique ordered key set should be idempotent"                       prop_identity_unique_ordered
  prop "the result of lookup should always be the same size as the query"    prop_lookup_length_idempotent
  prop "given non-overlapping keys, we should always find the input values"  prop_lookup_true

prop_null_empty ∷ Bool
prop_null_empty = null empty ≡ True

prop_singleton_size ∷ [(Char,Int)] → Bool
prop_singleton_size xs = size (singleton xs) ≡ L.length xs

prop_identity_unique_ordered ∷ [[Int]] → Bool
prop_identity_unique_ordered vss = (L.null vss) ∨ (toList (fromList kvs) ≡ kvs)
  where kvs = zipMV vss' ['a'..]
        vss' = L.filter (not ∘ L.null) vss

prop_lookup_length_idempotent ∷ [[(Char,Int)]] → [Char] → Bool
prop_lookup_length_idempotent kvs ks = L.length ks ≡ L.length (lookup ks (fromList kvs))

prop_lookup_true ∷ [[Int]] → Bool
prop_lookup_true vss = all foo kvss
  where tree = fromList kvss
        foo ∷ [(Char, Int)] → Bool
        foo kvs = fmap (Just ∘ snd) kvs ≡ lookup (fmap fst kvs) tree
        kvss = zipMV vss ['a'..]

-- this is a hack. should be replace by a [[(k,v)]] generator which can guarantee non-overlapping keys
zipMV ∷ [[α]] → [β] → [[(β, α)]]
zipMV [] _ = []
zipMV (vs:vss) ks = L.zip ks' vs : zipMV vss ks''
  where (ks', ks'') = L.splitAt (L.length vs) ks
