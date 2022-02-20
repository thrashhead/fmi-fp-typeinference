module Inferer where

import Control.Monad.Except
import Control.Monad.State
import Helper
import Types

-- CONTEXT
type Context = [(VarName, Polytype)]

-- adds new pair to the context

-- why are you pushing at the end?
pushCtx :: VarName -> Polytype -> Context -> Context
pushCtx vn pt ctxt = ctxt ++ [(vn, pt)] -- foldr (:) [(vn, pt)]

-- SUBSTITUTION
-- use a function!
type Substitution' = VarName -> Type

--substitute' :: Substitution' -> Type -> Type
--substitute' sub (TypeVar s) = sub s
--substitute' sub (TypeFunc ty ty') = TypeFunc (sub ty) (sub ty')
type Substitution = [(VarName, Type)]

addX :: Substitution' -> Substitution'
addX sub =
  \var ->
    if var == "x"
      then TypeVar "x"
      else sub var

-- applies substitution to a type
substitute :: Substitution -> Type -> Type
substitute sub (TypeVar var) =
  case findMaybe var sub of
    Just ty -> ty
    Nothing -> TypeVar var
substitute sub (TypeFunc fun arg) =
  TypeFunc (substitute sub fun) (substitute sub arg)

-- applies substitution to a polytype
substitutePoly :: Substitution -> Polytype -> Polytype
substitutePoly sub pt = polytypeFrom boundVars (substituteOnly freeVars sub ty)
  where
    ty = typeOf pt
    boundVars = boundVarsOf pt
    typeVars = typeVarsOf ty
    freeVars = differ typeVars boundVars

-- applies substitution only to specific vars in a type
substituteOnly :: [VarName] -> Substitution -> Type -> Type
substituteOnly vars sub (TypeVar var)
  | var `elem` vars = substitute sub (TypeVar var)
  | otherwise = TypeVar var
substituteOnly vars sub (TypeFunc binder body) = TypeFunc subBinder subBody
  where
    subBinder = substituteOnly vars sub binder
    subBody = substituteOnly vars sub body

-- applies substitution to a context
substituteCtx :: Substitution -> Context -> Context
substituteCtx sub = mapToSecond (substitutePoly sub)

-- composes two substitutions favouring the first one
compose :: Substitution -> Substitution -> Substitution
compose lhs rhs = joinDistinctLeft (mapToSecond (substitute rhs) lhs) rhs

-- NAME GENERATOR
-- use mtl style monads
type NameGenerator a = ExceptT String (State Int) a

-- generates new variable names
newvar :: NameGenerator Type
newvar = do
  uniqueCounter <- get
  put (uniqueCounter + 1)
  pure (TypeVar $ "t" <> show uniqueCounter)

runNG :: NameGenerator a -> (Either String a, Int)
runNG ng = runState (runExceptT ng) 0

-- INFERENCE ALGORITHM

-- infers an expression
infer :: Exp -> String
infer expr = do
  let (res, _) = runNG (extractType expr)
  case res of
    Left err -> err
    Right t -> show (generalize [] t)
  where
    extractType :: Exp -> NameGenerator Type
    extractType expression = do
      (s, t) <- inferCtx [] expression
      pure (substitute s t)

-- infers an expression, given a context
inferCtx :: Context -> Exp -> NameGenerator (Substitution, Type)
inferCtx ctx (ExpVar var) =
  case findMaybe var ctx of
    Just pt -> do
      ty <- instantiate pt
      pure ([], ty)
    Nothing -> throwError ("unbound variable" ++ var)
inferCtx ctx (ExpLam binder body) = do
  tyBinder <- newvar
  let bindCtx = pushCtx binder (Dot tyBinder) ctx
  (subBody, tyBody) <- inferCtx bindCtx body
  let tySubBinder = substitute subBody tyBinder
  pure (subBody, TypeFunc tySubBinder tyBody)
inferCtx ctx (ExpApp fun arg) = do
  tyRes <- newvar
  (subFun, tyFun) <- inferCtx ctx fun
  (subArg, tyArg) <- inferCtx (substituteCtx subFun ctx) arg
  subRes <- unify (substitute subArg tyFun) (TypeFunc tyArg tyRes)
  pure (subRes `compose` subArg `compose` subFun, substitute subRes tyRes)

-- from a given polytype, returns the monotype with the bound
-- variables renamed to previously unused ones
instantiate :: Polytype -> NameGenerator Type
instantiate pt = do
  newVars <- traverse (const newvar) boundVars
  let mapping = zip boundVars newVars
  pure $ substitute mapping ty
  where
    boundVars = boundVarsOf pt
    ty = typeOf pt

-- given a context and a type, returns a polytype, where the
-- free variables variables are bounded
generalize :: Context -> Type -> Polytype
generalize ctx ty = polytypeFrom freeVars ty
  where
    freeVars = differ (typeVarsOf ty) (map fst ctx)

-- return the most general substitution in order to unify two types
unify :: Type -> Type -> NameGenerator Substitution
unify (TypeFunc tFun tArg) (TypeFunc uFun uArg) = do
  subFun <- unify tFun uFun
  subArg <- unify (substitute subFun tArg) (substitute subFun uArg)
  pure $ compose subArg subFun
unify (TypeVar n) var = var `union` n
unify var (TypeVar n) = var `union` n

-- returns the most general substitution in order to unify
-- a type with a variable
union :: Type -> VarName -> NameGenerator Substitution
union t n
  | t == TypeVar n = pure []
  | n `elem` typeVarsOf t = throwError "occurs check failed!"
  | otherwise = pure [(n, t)]
