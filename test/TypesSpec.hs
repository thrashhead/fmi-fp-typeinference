{-# LANGUAGE BlockArguments #-}
module TypesSpec( typesSpec ) where

import Types
import HelperSpec ( equalLists )
import Test.Hspec

typesSpec :: Spec
typesSpec = describe "Types.hs" do
    typeOfSpec
    varsOfSpec
    typeVarsOfSpec
    boundVarsOfSpec
    polytypeFromSpec

-- typeOf :: Polytype -> Type
typeOfSpec :: Spec
typeOfSpec = describe "typeOf" do
    it "works with simple polytype" do
        let poly = Dot (TypeVar "x")

        let res = typeOf poly
        let exp = TypeVar "x"
        exp `shouldBe` res
    
    it "works with one quantified variable" do
        let poly = Forall "x" (Dot (TypeVar "x"))

        let res = typeOf poly
        let exp = TypeVar "x"
        exp `shouldBe` res

    it "retruns monotype id from polytype id" do
        let poly = Forall "x" (Dot (TypeFunc (TypeVar "x") (TypeVar "x")))
        let res = typeOf poly
        let exp = TypeFunc (TypeVar "x") (TypeVar "x")
        exp `shouldBe` res

-- varsOf :: Polytype -> [VarName]
varsOfSpec :: Spec
varsOfSpec = describe "varsOf" do
    it "works in the simplest case" do
        let poly = Dot (TypeVar "x")

        let res = varsOf poly
        let exp = ["x"]
        equalLists exp res `shouldBe` True
    
    it "takes quantified variable" do
        let poly = Forall "y" (Dot (TypeVar "x"))

        let res = varsOf poly
        let exp = ["x", "y"]
        equalLists exp res `shouldBe` True
    
    it "returns singleton list from polytype id" do
        let poly = Forall "x" (Dot (TypeFunc (TypeVar "x") (TypeVar "x")))

        let res = varsOf poly
        let exp = ["x"]
        equalLists exp res `shouldBe` True

    it "returns the two vars from apply" do
        let poly = Forall "x" (Forall "y" (Dot (TypeFunc (TypeVar "x") (TypeVar "y"))))

        let res = varsOf poly
        let exp = ["x", "y"]
        equalLists exp res `shouldBe` True

    it "returns the two vars uniquely from twice" do
        let poly = Forall "x" (Forall "y" (Dot (TypeFunc (TypeFunc (TypeVar "x") (TypeVar "x")) (TypeVar "y"))))

        let res = varsOf poly
        let exp = ["x", "y"]
        equalLists exp res `shouldBe` True

-- typeVarsOf :: Type -> [VarName]
typeVarsOfSpec :: Spec
typeVarsOfSpec = describe "typeVarsOf" do
    it "works in the simplest case" do
        let ty = TypeVar "x"

        let res = typeVarsOf ty
        let exp = ["x"]
        equalLists exp res `shouldBe` True

    it "returns set of variables" do
        let ty = TypeFunc (TypeVar "x") (TypeVar "x")

        let res = typeVarsOf ty
        let exp = ["x"]
        equalLists exp res `shouldBe` True
    
    it "returns all variables" do
        let ty = TypeFunc (TypeVar "x") (TypeVar "y")

        let res = typeVarsOf ty
        let exp = ["x", "y"]
        equalLists exp res `shouldBe` True

-- boundVarsOf :: Polytype -> [VarName]
boundVarsOfSpec :: Spec
boundVarsOfSpec = describe "boundVarsOf" do
    it "returns empty list for Dot" do
        let poly = Dot $ TypeVar "x"

        let res = boundVarsOf poly
        let exp = []
        equalLists exp res `shouldBe` True
    
    it "returns singleton" do 
        let poly = Forall "x" (Dot $ TypeVar "x")

        let res = boundVarsOf poly
        let exp = ["x"]
        equalLists exp res `shouldBe` True

    it "returns list of two" do
        let poly = Forall "x" (Forall "y" (Dot $ TypeVar "x"))

        let res = boundVarsOf poly
        let exp = ["x", "y"]
        equalLists exp res `shouldBe` True

-- polytypeFrom :: [VarName] -> Type -> Polytype
polytypeFromSpec :: Spec
polytypeFromSpec = describe "polytypeFrom" do
    it "returns Dot" do
        let bound = []
        let ty = TypeVar "x"

        let res = polytypeFrom bound ty
        let exp = Dot $ TypeVar "x"
        exp `shouldBe` res

    it "returns one quantifier" do
        let bound = ["x"]
        let ty = TypeVar "x"

        let res = polytypeFrom bound ty
        let exp = Forall "x" (Dot $ TypeVar "x")
        exp `shouldBe` res

    it  "returns two quantifiers" do 
        let bound = ["x", "y"]
        let ty = TypeVar "x"

        let res = polytypeFrom bound ty
        let exp = Forall "x" (Forall "y" (Dot $ TypeVar "x"))
        exp `shouldBe` res

   

    

    
    

    

    
    

    

    
    

    

   

    
   

    

    

    