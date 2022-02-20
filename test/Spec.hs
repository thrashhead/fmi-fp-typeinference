{-# LANGUAGE BlockArguments #-}
module Main where

import HelperSpec( helperSpec )
import TypesSpec( typesSpec )
import InfererSpec( infererSpec )

import Test.Hspec

main :: IO()
main = hspec helperSpec
    <> hspec typesSpec
    <> hspec infererSpec
