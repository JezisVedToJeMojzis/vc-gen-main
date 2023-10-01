module Expr
  ( Expr (..)
  , BinOp (..)
  , Pred (..)

  , Vars (..)
  , Subable (..)
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Expressions are either of type integer or array
data Expr a
  = Var a
  | Array a
  | Const Integer
  | BinOp BinOp (Expr a) (Expr a)
  | Select (Expr a) (Expr a)
  | Store (Expr a) (Expr a) (Expr a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Binary expression operations.
data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  deriving (Eq, Ord, Show)

-- | Predicates are of type boolean
data Pred a
  = Expr a :==: Expr a
  -- ^ Equals
  | Expr a :>=: Expr a
  -- ^ Less than or equals
  | Expr a :<=: Expr a
  -- ^ Greater than or equals
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class Vars f where
  vars :: Ord a => f a -> Set (Expr a)

instance Vars Expr where
  vars = \case
    v@(Var _) -> Set.singleton v
    a@(Array _) -> Set.singleton a
    Const _ -> mempty
    BinOp _ lhs rhs -> vars lhs <> vars rhs
    Select array index -> vars array <> vars index
    Store array index expr -> mconcat [vars array, vars index, vars expr]

instance Vars Pred where
  vars = \case
    lhs :==: rhs -> vars lhs <> vars rhs
    lhs :>=: rhs -> vars lhs <> vars rhs
    lhs :<=: rhs -> vars lhs <> vars rhs

-- | Substitues an expression for the passed variable
class Subable s where
  subst :: Eq a => a -> Expr a -> s a -> s a

instance Subable Expr where
  subst var expr = substExpr -- substitute var with expr
    where
      substExpr (Var v) = if v == var then expr else Var v 
      substExpr (Array a) = if a == var then expr else Array a 
      substExpr (Const n) = Const n -- return original const
      substExpr (BinOp operation lhs rhs) = BinOp operation (substExpr lhs) (substExpr rhs) 
      substExpr (Select array index) = Select (substExpr array) (substExpr index)
      substExpr (Store array index val) = Store (substExpr array) (substExpr index) (substExpr val)
  
instance Subable Pred where
  subst var expr pred = case pred of
    (lhs :==: rhs) -> (substVarWithExpr (:==:) lhs rhs) 
    (lhs :>=: rhs) -> (substVarWithExpr (:>=:) lhs rhs)
    (lhs :<=: rhs) -> (substVarWithExpr (:<=:) lhs rhs)
    where
      substVarWithExpr operation lhs rhs = -- substitutes variables in the operation with expr for both lhs and rhs
        let lhs' = subst var expr lhs
            rhs' = subst var expr rhs
        in (lhs' `operation` rhs') -- applying operation to new lhs and rhs