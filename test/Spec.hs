{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Data.GraphQL
import Data.GraphQL.Types
import Data.Maybe (fromJust)

p s =
    case (parseGraphQL s) of
        Right r -> r
        Left l -> error $ show l

q :: String -> [String] -> GQLDocument
q name fs = GQLQuery (Just name) [] $ fmap (\field -> GQLField field []) fs

t :: String -> GQLType
t = GQLType . GQLNamedType

main :: IO ()
main = hspec $ do
  describe "parsing valid queries" $ do
    describe "shorthand syntax" $ do
      it "simple" $ do
        (p "{ name }") `shouldBe`
         GQLQuery Nothing [] [GQLField "name" []]

    describe "simple queries" $ do
      it "with one fields" $ do
        (p "query foo { abc_  }") `shouldBe`
         q "foo" ["abc_"]
      it "with two fields" $ do
        (p "query foo { __firstName\nlastName }") `shouldBe`
         q "foo" ["__firstName", "lastName"]

    describe "queries with variables" $ do
      it "one variable" $ do
        (p "query foo($bar : String) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariable "bar" (t "String") Nothing] [GQLField "abc_" []]
      it "two variables" $ do
        (p "query foo($bar : String $foo: Integer) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariable "bar" (t "String") Nothing, GQLVariable "foo" (t "Integer") Nothing] [GQLField "abc_" []]
      it "list types" $ do
        (p "query foo($bar : [String]) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariable "bar" (GQLType (GQLListType "String")) Nothing] [GQLField "abc_" []]
      it "nullable named types" $ do
        (p "query foo($bar : [String] !) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariable "bar" (GQLNullableType (GQLListType "String")) Nothing] [GQLField "abc_" []]
      it "nullable list types" $ do
        (p "query foo($bar : [String] !) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariable "bar" (GQLNullableType (GQLListType "String")) Nothing] [GQLField "abc_" []]

    describe "nested queries" $ do
      it "one level deep" $ do
        (p "query foo { parent { child } }") `shouldBe`
         GQLQuery (Just "foo") [] [GQLField "parent" [GQLField "child" []]]
      it "two levels deep" $ do
        (p "query foo { parent { child { anotherCild }} }") `shouldBe`
         GQLQuery (Just "foo") [] [GQLField "parent" [GQLField "child" [GQLField "anotherCild" []]]]
