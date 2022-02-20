{-# LANGUAGE InstanceSigs #-}

module Types where

import Helper (joinDistinctLeft)

-- use a newtype
type VarName = String

data Type
  = TypeVar VarName
  | TypeFunc Type Type
  deriving (Eq, Ord, Show)

typeOf :: Polytype -> Type
typeOf (Dot ty) = ty
typeOf (Forall _ rest) = typeOf rest

varsOf :: Polytype -> [VarName]
varsOf pt = joinDistinctLeft boundVars typeVars
  where
    boundVars = boundVarsOf pt
    typeVars = typeVarsOf $ typeOf pt

typeVarsOf :: Type -> [VarName]
typeVarsOf (TypeVar var) = [var]
typeVarsOf (TypeFunc t1 t2) = joinDistinctLeft (typeVarsOf t1) (typeVarsOf t2)

boundVarsOf :: Polytype -> [VarName]
boundVarsOf (Dot _) = []
boundVarsOf (Forall bound rest) = bound : boundVarsOf rest

data Exp
  = ExpVar VarName
  | ExpApp Exp Exp
  | ExpLam VarName Exp
  deriving (Eq, Ord, Show)

-- use a nameless representation
-- безименно представяне
data Polytype
  = Forall VarName Polytype
  | Dot Type
  deriving (Eq, Show)

-- Forall "x" (Dot (TypeVar "x")) == Forall "y" (Dot (TypeVar "y"))

-- \x. \y. x
-- \.\.1
-- \x. \y. z
-- \.\. 2

-- \forall x. x
-- \forall y. y
-- \forall. 0 == \forall. 0

-- \forall x. x ==
-- \forall. 0

-- \forall x. \forall y. x
-- \forall. \forall. 1

-- \forall x. y

-- \forall x. y
-- y -> x
-- \forall x. x

polytypeFrom :: [VarName] -> Type -> Polytype
polytypeFrom xs ty = foldr Forall (Dot ty) xs
