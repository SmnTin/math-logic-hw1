module Types where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Foldable (Foldable(foldl'))


type Symb = String


infixr 7 :/\:
infixr 6 :\/:
infixr 5 :<->:, :-->

data Formula = Var Symb
             | Neg Formula            -- negation
             | Formula :/\: Formula   -- and
             | Formula :\/: Formula   -- or
             | Formula :--> Formula   -- implication
             | Formula :<->: Formula  -- bi implication
    deriving (Eq, Show)

class FormulaForm form where
    fromForm :: form -> Formula


data Literal = Lit Symb
             | NLit Symb

fromLiteral :: Literal -> Formula
fromLiteral (Lit x) = Var x
fromLiteral (NLit x) = Neg (Var x)


newtype Clause = Clause (NonEmpty Literal)
fromClause binder (Clause literals) = foldr1 binder $ fmap fromLiteral literals

newtype CNF = CNF (NonEmpty Clause)
newtype DNF = DNF (NonEmpty Clause)


infixr 7 :/\:*
infixr 6 :\/:*

data NNF = NNFVar Literal
         | NNF :/\:* NNF
         | NNF :\/:* NNF


instance FormulaForm CNF where
    fromForm (CNF clauses) = foldr1 (:/\:) $ fromClause (:\/:) <$> clauses 

instance FormulaForm DNF where
    fromForm (DNF clauses) = foldr1 (:\/:) $ fromClause (:/\:) <$> clauses 

instance FormulaForm NNF where
    fromForm (NNFVar lit) = fromLiteral lit
    fromForm (a :/\:* b) = fromForm a :/\: fromForm b
    fromForm (a :\/:* b) = fromForm a :\/: fromForm b
