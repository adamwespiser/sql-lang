{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}

module Select.Expression
  ( Expression (..)
  , Named (..)
  , As
  ) where

-- | Untyped AST for SQL expressions
data Expression variable
  = LiteralBool Bool
  | LiteralInt Int
  | LiteralReal Double
  | LiteralString String
  | Column variable
  | Not (Expression variable)
  | And (Expression variable) (Expression variable)
  | Or  (Expression variable) (Expression variable)
  | Equ (Expression variable) (Expression variable)
  | Neq (Expression variable) (Expression variable)
  | Gt  (Expression variable) (Expression variable)
  | Gte (Expression variable) (Expression variable)
  | Lt  (Expression variable) (Expression variable)
  | Lte (Expression variable) (Expression variable)
  | Neg (Expression variable)
  | Add (Expression variable) (Expression variable)
  | Sub (Expression variable) (Expression variable)
  | Mul (Expression variable) (Expression variable)
  | Div (Expression variable) (Expression variable)
  | Mod (Expression variable) (Expression variable)
  deriving
    ( Read
    , Show
    , Eq
    , Functor
    , Foldable
    , Traversable
    )

-- | `Named` used for naming objects and bringing them into scope
data Named scope x = AS x scope
  deriving
    ( Read
    , Show
    , Eq
    , Functor
    , Foldable
    , Traversable
    )

-- | `As` is `Named` with type variables flipped
type As x scope = Named scope x
