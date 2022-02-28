{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Data.List
import Data.Monoid
import Control.Monad
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Forms


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ testEquivalence ]


instance Arbitrary Formula where
    arbitrary = sized genFormula where
        genFormula 0         = fmap Var arbitrary
        genFormula n | n > 0 = oneof 
            [ fmap Var arbitrary
            , liftM2 (:/\:) subtree subtree
            , liftM2 (:\/:) subtree subtree
            , liftM2 (:-->) subtree subtree
            , liftM2 (:<->:) subtree subtree
            ]
            where subtree = genFormula $ min 3 $ n `div` 2
        genFormula _ = undefined


testEquivalence :: TestTree
testEquivalence = testGroup "Formulas equivalence" [
        QC.testProperty "NNF equivalent to original" $
            \f -> equivalent f $ fromForm $ (toForm :: Formula -> NNF) f
      , QC.testProperty "CNF equivalent to original" $
            \f -> equivalent f $ fromForm $ (toForm :: Formula -> CNF) f
      , QC.testProperty "DNF equivalent to original" $
            \f -> equivalent f $ fromForm $ (toForm :: Formula -> DNF) f
    ]


inputs :: [Symb] -> [Interpretation]
inputs = mapM (\s -> [(s,True), (s,False)])

equivalent :: Formula -> Formula -> Bool
equivalent a b = getAll $ foldMap testOnInterpret $ inputs $ support a `union` support b
    where testOnInterpret int = All $ evaluate int a == evaluate int b