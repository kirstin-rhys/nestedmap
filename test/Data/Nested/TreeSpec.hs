{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Data.Nested.TreeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.List as L
import Prelude ((+))
import Data.Char (Char)
import Data.Int (Int)
import Data.Eq.Unicode ((≡))
import Data.Function (($))
import Data.Bool (Bool(..))
import Data.Nested.Tree ( empty
                        , null
                        , singleton
                        , size
                        )

spec ∷ Spec
spec = describe "Tree" $ do
       prop "null empty should always be true" prop_null_empty
       prop "the size of a singleton tree should be the length of the argument plus one" prop_singleton_size

prop_null_empty ∷ Int → Bool
prop_null_empty x = null (empty x) ≡ True

prop_singleton_size ∷ Char → [(Int,Char)] → Bool
prop_singleton_size x xs = size (singleton x xs) ≡ L.length xs + 1
