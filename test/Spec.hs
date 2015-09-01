{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Lib
import Data.Maybe (fromJust)

main :: IO ()
main = hspec $ do
  describe "parsing valid queries" $ do
    it "can parse a query with two fields" $ do
      (fromJust $ parseGraphQL "{ __firstName\nlastName }") `shouldBe` GQLQuery [GQLSelection "__firstName", GQLSelection "lastName"]
