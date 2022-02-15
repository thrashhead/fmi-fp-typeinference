{-# LANGUAGE InstanceSigs #-}
module Types where

import Helper ( joinDistinctLeft )

type VarName = String

data Type = 
    TypeVar VarName |
    TypeFunc Type Type
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

data Exp =
    ExpVar VarName |
    ExpApp Exp Exp |
    ExpLam VarName Exp
  deriving (Eq, Ord, Show)

data Polytype = 
    Forall VarName Polytype |
    Dot Type 
  deriving (Eq, Show)

polytypeFrom :: [VarName] -> Type -> Polytype
polytypeFrom xs ty = foldr Forall (Dot ty) xs



