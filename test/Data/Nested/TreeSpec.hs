{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Data.Nested.TreeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.List as L
import Prelude ((+))
import Data.Char (Char)
import Data.Int (Int)
import Data.Tuple (snd)
import Data.Eq.Unicode ((≡))
import Data.Function (($))
import Data.Bool (Bool(..))
import Data.Nested.Tree ( empty
                        , null
                        , singleton
                        , size
                        , fromList
                        , lookup
                        )

spec ∷ Spec
spec = describe "Tree" $ do
       prop "null empty should always be true"                                           prop_null_empty
       prop "the size of a singleton tree should be the length of the argument plus one" prop_singleton_size
       prop "the result of lookup should always be one more than the the query size"     prop_lookup_length_idempotent

prop_null_empty ∷ Int → Bool
prop_null_empty v = null (empty v) ≡ True

prop_singleton_size ∷ Char → [(Int,Char)] → Bool
prop_singleton_size v kvs = size (singleton v kvs) ≡ L.length kvs + 1

prop_lookup_length_idempotent ∷ Int → [[(Char,Int)]] → [Char] → Bool
prop_lookup_length_idempotent v kvs ks = L.length ks ≡ L.length (snd (lookup ks (fromList v kvs)))
