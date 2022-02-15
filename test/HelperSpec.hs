{-# LANGUAGE BlockArguments #-}
module HelperSpec( helperSpec, equalLists ) where

import Helper
import Data.List

import Test.Hspec


helperSpec :: Spec
helperSpec = describe "Helper.hs" $ do
    differSpec
    mapToSecondSpec
    joinDistinctSpec
    findMaybeSpec


-- differ :: Eq a => [a] -> [a] -> [a]
differSpec :: Spec
differSpec = describe "differ" do
    it "returns lhs when empty crossection" do
        equalLists [1, 2] (differ [1, 2] [3, 4]) `shouldBe` True

    it "works with single element crossection" do
        equalLists [1] (differ [1, 2] [2, 3]) `shouldBe` True

    it "works with subset / returns empty list" do
        equalLists [] (differ [1, 2] [1, 2, 3, 4]) `shouldBe` True

-- mapToSecond :: (b -> c) -> [(a, b)] -> [(a, c)]
mapToSecondSpec :: Spec
mapToSecondSpec = describe "mapToSecond" do
    it "works with id" do
        let list = [("a", 1), ("b", 2)]

        equalLists [("a",1 ), ("b", 2)] (mapToSecond id list) `shouldBe` True
    it "works when succed with succ" do
        let list = [("a", 1), ("b", 2)]

        equalLists [("a", 2), ("b", 3)] (mapToSecond succ list) `shouldBe` True

-- joinDistinct :: Eq a => [a] -> [a] -> [a]
joinDistinctSpec :: Spec
joinDistinctSpec = describe "joinDistinct" do
    it "works with empty list" do
        let lhs = [1, 2]
        let rhs = []

        let res = joinDistinctLeft lhs rhs
        let exp = [1, 2]
        equalLists exp res `shouldBe` True

    it "union with empty cressection" do 
        let lhs = [1, 2]
        let rhs = [3, 4]

        let res = joinDistinctLeft lhs rhs
        let exp = [1, 2, 3, 4]
        equalLists exp res `shouldBe` True 

    it "remove one dublicate once" do
        let lhs = [1, 2]
        let rhs = [2, 3]

        let res = joinDistinctLeft lhs rhs
        let exp = [1, 2, 3]
        equalLists exp res `shouldBe` True

    it "saves the rhs list" do
        let lhs = [1, 2]
        let rhs = [2, 2, 3]

        let res = joinDistinctLeft lhs rhs
        let exp = [1, 2, 2, 3]
        equalLists exp res `shouldBe` True
    
-- findMaybe :: Eq a => a -> [(a, b)] -> Maybe b
findMaybeSpec :: Spec
findMaybeSpec = describe "findMaybe" do
    it "simplest case returns Just" do
        let list = [(1, "a"), (2, "b")] 

        let res = findMaybe 1 list
        let exp = Just "a"
        exp `shouldBe` res

    it "simplest case returns Nothing" do
        let list = [(1, "a"), (2, "b")]

        let res = findMaybe 3 list
        let exp = Nothing
        exp `shouldBe` res

    it "returns first found" do
        let list = [(1, "a"), (1, "A"), (2, "b")]

        let res = findMaybe 1 list
        let exp = Just "a"
        exp `shouldBe` res


equalLists :: (Eq a, Ord a) => [a] -> [a] -> Bool
equalLists xs ys = xs' == ys'
    where
        xs' = sort xs
        ys' = sort ys