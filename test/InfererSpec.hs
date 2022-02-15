{-# LANGUAGE BlockArguments #-}
module InfererSpec( infererSpec ) where

import Inferer
import Types (Type(TypeVar, TypeFunc), Polytype(Dot, Forall), Exp(ExpVar, ExpLam, ExpApp))

import Test.Hspec

infererSpec :: Spec
infererSpec = describe "Inferer.hs" do
    pushCtxSpec
    substituteSpec
    substitutePolySpec
    substituteOnlySpec
    substituteCtxSpec
    composeSpec
    newvarSpec
    -- runNGSpec
    inferSpec
    -- inferCtxSpec
    -- instantiateSpec
    generalizeSpec
    -- unifySpec
    -- unionSpec

-- pushCtx :: VarName -> Polytype -> Context -> Context
pushCtxSpec :: Spec
pushCtxSpec = describe "pushCtx" do
    it "pushes correctly into empty ctx" do
        let poly = Dot $ TypeVar "x"
        let ctx = []

        let res = pushCtx "x" poly ctx
        let exp = [("x", Dot $ TypeVar "x")]
        exp `shouldBe` res

    it "pushes correctly into non-empty ctx" do
        let poly = Dot $ TypeVar "x"
        let ctx = [("y", Dot $ TypeVar "y")]

        let res = pushCtx "x" poly ctx
        let exp = [("y", Dot $ TypeVar "y"), ("x", Dot $ TypeVar "x")]
        exp `shouldBe` res


-- SUBSTITUTION

-- substitute :: Substitution -> Type -> Type
substituteSpec :: Spec
substituteSpec = describe "substitute" do
    it "substitutes 'x' for 'a'" do
        let subst = [("x", TypeVar "a"), ("y", TypeFunc (TypeVar "a") (TypeVar "a"))]

        let res = substitute subst (TypeVar "x")
        let exp = TypeVar "a"
        exp `shouldBe` res

    it "substitutes 'x' for 'a -> a'" do
        let subst = [("x", TypeVar "a"), ("y", TypeFunc (TypeVar "a") (TypeVar "a"))]

        let res = substitute subst (TypeVar "y")
        let exp = TypeFunc (TypeVar "a") (TypeVar "a")
        exp `shouldBe` res

-- substitutePoly :: Substitution -> Polytype -> Polytype
substitutePolySpec :: Spec
substitutePolySpec = describe "substitutePoly" do
    it "substitutes IT [] type" do
        let subst = [("x", TypeVar "a")]

        let res = substitutePoly subst (Dot (TypeVar "x"))
        let exp = Dot (TypeVar "a")
        exp `shouldBe` res

    it "substitutes IT [] type" do
        let subst = [("x", TypeVar "a")]

        let res = substitutePoly subst (Forall "x" (Dot (TypeVar "x")))
        let exp = Forall "x" (Dot (TypeVar "x"))
        exp `shouldBe` res

    it "substitutes IT [] type" do
        let subst = [("x", TypeVar "a")]

        let res = substitutePoly subst (Dot (TypeFunc (TypeVar "x") (TypeVar "x")))
        let exp = Dot (TypeFunc (TypeVar "a") (TypeVar "a"))
        exp `shouldBe` res

-- substituteOnly :: [VarName] -> Substitution -> Type -> Type
substituteOnlySpec :: Spec
substituteOnlySpec = describe "substituteOnly" do
    it "works on empty list" do
        let list = []
        let sub = [("x", TypeVar "y")]

        substituteOnly list sub (TypeVar "x") `shouldBe` TypeVar "x"

    it "works on singleton" do
        let list = ["x"]
        let sub = [("x", TypeVar "y")]

        substituteOnly list sub (TypeVar "x") `shouldBe` TypeVar "y"

    it "works when vars in list arent in type" do
        let list = ["z"]
        let sub = [("x", TypeVar "y")]

        substituteOnly list sub (TypeFunc (TypeVar "x") (TypeVar "x")) `shouldBe` TypeFunc (TypeVar "x") (TypeVar "x")

    it "works on type functions" do
        let list = ["x"]
        let sub = [("x", TypeVar "y")]

        substituteOnly list sub (TypeFunc (TypeVar "x") (TypeVar "x")) `shouldBe` TypeFunc (TypeVar "y") (TypeVar "y")

-- substituteCtx :: Substitution -> Context -> Context
substituteCtxSpec :: Spec
substituteCtxSpec = describe "substituteCtx" do
    it "works on Dot Polytype " do
        let subst = [("x", TypeVar "a")]
        let context = [("v", Dot (TypeVar "x"))]

        substituteCtx subst context `shouldBe` [("v", Dot (TypeVar "a"))]

    it "works on Forall Polytype" do
        let subst = [("x", TypeVar "a")]
        let context = [("v", Forall "x" (Dot (TypeVar "x")))]

        substituteCtx subst context `shouldBe` [("v", Forall "x" (Dot (TypeVar "x")))]

-- compose :: Substitution -> Substitution -> Substitution
composeSpec :: Spec
composeSpec = describe "compose" do
    it "doesnt compose uncomposable substitutions" do
        let lhs = [("x", TypeVar "a")]
        let rhs = [("y", TypeVar "b")]

        compose lhs rhs `shouldBe` [("x", TypeVar "a"), ("y", TypeVar "b")]

    it "works on basic composition" do
        let lhs = [("y", TypeVar "x")]
        let rhs = [("x", TypeVar "a")]

        compose lhs rhs `shouldBe`  [("y", TypeVar "a"), ("x", TypeVar "a")]

    it "doesnt dublicate" do
        let lhs = [("x", TypeVar "a"), ("y", TypeVar "x")]
        let rhs = [("x", TypeVar "a")]

        compose lhs rhs `shouldBe` [("y", TypeVar "a"), ("x", TypeVar "a")]




-- NAME GENERATOR

-- newvar :: NameGenerator Type
newvarSpec :: Spec
newvarSpec = describe "newvar" do
    it "generates new var" do
        let ng = newvar
        let (_, res) = runNG ng

        res `shouldBe` 1

-- -- runNG :: NameGenerator a -> (Either String a, Int)
-- runNGSpec :: Spec
-- runNGSpec = describe "runNG" do
--     it "works" do
--         1 `shouldBe` 1

-- INFERENCE ALGORITHM

-- infer :: Exp -> String
inferSpec :: Spec
inferSpec = describe "infer" do
    it "works with id" do
        let res = infer (ExpLam "x" (ExpVar "x"))
        let exp = "Forall \"t0\" (Dot (TypeFunc (TypeVar \"t0\") (TypeVar \"t0\")))"
        res `shouldBe` exp

    it "works with const" do
        let res = infer (ExpLam "x" (ExpLam "y" (ExpVar "x")))
        let exp = "Forall \"t1\" (Forall \"t0\" (Dot (TypeFunc (TypeVar \"t0\") (TypeFunc (TypeVar \"t1\") (TypeVar \"t0\")))))"
        res `shouldBe` exp


    it "works with apply" do
        let res = infer (ExpLam "f" (ExpLam "x" (ExpApp (ExpVar "f")(ExpVar "x"))))
        let exp = "Forall \"t1\" (Forall \"t2\" (Dot (TypeFunc (TypeFunc (TypeVar \"t1\") (TypeVar \"t2\")) (TypeFunc (TypeVar \"t1\") (TypeVar \"t2\")))))"
        res `shouldBe` exp

    it "works with more compicated expression" do
        let res = infer (ExpLam "x" (ExpLam "y" (ExpLam "z" (ExpApp (ExpApp (ExpVar "x")(ExpVar "y"))(ExpApp (ExpVar "y")(ExpVar "z"))))))
        let exp = "Forall \"t5\" (Forall \"t2\" (Forall \"t3\" (Dot (TypeFunc (TypeFunc (TypeFunc (TypeVar \"t2\") (TypeVar \"t5\")) (TypeFunc (TypeVar \"t5\") (TypeVar \"t3\"))) (TypeFunc (TypeFunc (TypeVar \"t2\") (TypeVar \"t5\")) (TypeFunc (TypeVar \"t2\") (TypeVar \"t3\")))))))"
        res `shouldBe` exp

-- -- inferCtx :: Context -> Exp -> NameGenerator (Substitution, Type)
-- inferCtxSpec :: Spec
-- inferCtxSpec = describe "inferCtx" do
--     it "works" do
--         1 `shouldBe` 1

-- -- instantiate :: Polytype -> NameGenerator Type
-- instantiateSpec :: Spec
-- instantiateSpec  = describe "instantiate" do
--     it "works" do
--         1 `shouldBe` 1

-- generalize :: Context -> Type -> Polytype
generalizeSpec :: Spec
generalizeSpec = describe "generalize" do
    it "returns the Forall of the type given empty context" do
        generalize [] (TypeVar "x") `shouldBe` Forall "x" (Dot (TypeVar "x"))
    it "returns the Forall of the function type given empty context" do
        generalize [] (TypeFunc (TypeVar "x") (TypeVar "x")) `shouldBe` Forall "x" (Dot (TypeFunc (TypeVar "x") (TypeVar "x")))
    it "generalizes with simple context" do
        generalize [("x", Dot $ TypeVar "x")] (TypeVar "x") `shouldBe` (Dot (TypeVar "x"))

-- -- unify :: Type -> Type -> NameGenerator Substitution
-- unifySpec :: Spec
-- unifySpec = describe "unify" do
--     it "works" do
--         1 `shouldBe` 1

-- -- union :: Type -> VarName -> NameGenerator Substitution
-- unionSpec :: Spec
-- unionSpec = describe "union" do
--     it "works" do
--         1 `shouldBe` 1
