{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}

module Select.Relation
  ( Relation (..)
  , RelationIdentifier
  ) where

import Select.Expression

infix 1 `FROM`  --loose
infixr 2 `UNION`
infix 3 `INNER_JOIN_ON`
infix 4 `WHERE` --tight

-- | Relation abstract syntax tree
data Relation scope variable table
  = TABLE table
  | FROM
    [Expression variable `As` scope] -- projection
    (Relation scope variable table)
  | WHERE
    (Relation scope variable table)
    (Expression variable) -- predicate
  | UNION (Relation scope variable table) (Relation scope variable table)
  | INNER_JOIN_ON
    (Relation scope variable table `As` scope)
    (Relation scope variable table `As` scope)
    (Expression (scope,variable)) -- predicate
  deriving (Read,Show,Eq,Functor,Foldable,Traversable)

-- | Identifier for a relation
type RelationIdentifier = Relation String String FilePath
