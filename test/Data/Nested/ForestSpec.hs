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
import Data.Bool (not)
import Data.Bool.Unicode ((∨))
import Data.Eq.Unicode ((≡))
import Data.Function (($))
import Data.Tuple (uncurry)
import Data.Functor (fmap)
import Data.Bool (Bool(..))
import Data.Nested.Forest ( empty
                          , null
                          , size
                          , singleton
                          , fromList
                          , toList
                          )

spec :: Spec
spec = describe "Forest" $ do
  prop "null empty should always be true"                                    prop_null_empty
  prop "the size of a singleton forest should be the length of the argument" prop_singleton_size
  prop "a unique ordered key set should be idempotent"                       prop_identity_unique_ordered

prop_null_empty ∷ Bool
prop_null_empty = null empty ≡ True

prop_singleton_size ∷ [(Int,Char)] → Bool
prop_singleton_size xs = size (singleton xs) ≡ L.length xs

prop_identity_unique_ordered ∷ [[Char]] → Bool
prop_identity_unique_ordered vss = (L.null vss) ∨ (toList (fromList kvs) ≡ kvs)
  where kvs = zipMV vss' [1..]
        vss' = L.filter (not ∘ L.null) vss

zipMV ∷ [[α]] → [β] → [[(β, α)]]
zipMV [] _ = []
zipMV (vs:vss) ks = L.zip ks' vs : zipMV vss ks''
  where (ks', ks'') = L.splitAt (L.length vs) ks
