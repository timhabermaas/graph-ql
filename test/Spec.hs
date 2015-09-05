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
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (t "String") Nothing] [GQLField "abc_" []]
      it "two variables" $ do
        (p "query foo($bar : String $foo: Integer) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (t "String") Nothing, GQLVariableDefinition (GQLVariable "foo") (t "Integer") Nothing] [GQLField "abc_" []]
      it "list types" $ do
        (p "query foo($bar : [String]) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLListType "String")) Nothing] [GQLField "abc_" []]
      it "nullable named types" $ do
        (p "query foo($bar : [String] !) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLNonNullType (GQLListType "String")) Nothing] [GQLField "abc_" []]
      it "nullable list types" $ do
        (p "query foo($bar : [String] !) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLNonNullType (GQLListType "String")) Nothing] [GQLField "abc_" []]

    describe "default values" $ do
      it "integer value" $ do
        (p "query foo($bar : [String] = -2 ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLListType "String")) (Just $ GQLIntValue (-2))] [GQLField "abc_" []]
      it "integer value (+)" $ do
        (p "query foo($bar : [String] = +2 ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLListType "String")) (Just $ GQLIntValue 2)] [GQLField "abc_" []]
      it "integer value 0" $ do
        (p "query foo($bar : [String] = 0 ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLListType "String")) (Just $ GQLIntValue 0)] [GQLField "abc_" []]
      it "boolean value true" $ do
        (p "query foo($bar : [Boolean] = true ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLListType "Boolean")) (Just $ GQLBooleanValue True)] [GQLField "abc_" []]
      it "boolean value false" $ do
        (p "query foo($bar : [Boolean] = false ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLListType "Boolean")) (Just $ GQLBooleanValue False)] [GQLField "abc_" []]
      it "empty array" $ do
        (p "query foo($bar : [Integer] = [] ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLListType "Integer")) (Just $ GQLListValue [])] [GQLField "abc_" []]
      it "integer array" $ do
        (p "query foo($bar : [Integer] = [ 2 -2] ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLListType "Integer")) (Just $ GQLListValue [GQLIntValue 2, GQLIntValue (-2)])] [GQLField "abc_" []]
      it "simple objects" $ do
        (p "query foo($bar : Object = { foo: 2 } ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLNamedType "Object")) (Just $ GQLObjectValue [GQLObjectField "foo" (GQLIntValue 2)])] [GQLField "abc_" []]
      it "nested objects" $ do
        (p "query foo($bar : Object = { foo: {bar: true, a: 3} } ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLNamedType "Object")) (Just (GQLObjectValue [GQLObjectField "foo" (GQLObjectValue [GQLObjectField "bar" (GQLBooleanValue True),GQLObjectField "a" (GQLIntValue 3)])]))] [GQLField "abc_" []]
      it "variable" $ do
        (p "query foo($bar : Integer = $n ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLNamedType "Integer")) (Just $ GQLVariableValue (GQLVariable "n"))] [GQLField "abc_" []]
      it "enum" $ do
        (p "query foo($bar : Integer = ACTIVATED ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLNamedType "Integer")) (Just $ GQLEnumValue "ACTIVATED")] [GQLField "abc_" []]
      it "float" $ do
        (p "query foo($bar : Float = 13.2 ) { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLNamedType "Float")) (Just $ GQLFloatValue 13.2)] [GQLField "abc_" []]
      it "empty string" $ do
        (p "query foo($bar : String = \"\") { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLNamedType "String")) (Just $ GQLStringValue "")] [GQLField "abc_" []]
      it "non-empty string" $ do
        (p "query foo($bar : String = \"content and spaces\") { abc_  }") `shouldBe`
         GQLQuery (Just "foo") [GQLVariableDefinition (GQLVariable "bar") (GQLType (GQLNamedType "String")) (Just $ GQLStringValue "content and spaces")] [GQLField "abc_" []]




    describe "nested queries" $ do
      it "one level deep" $ do
        (p "query foo { parent { child } }") `shouldBe`
         GQLQuery (Just "foo") [] [GQLField "parent" [GQLField "child" []]]
      it "two levels deep" $ do
        (p "query foo { parent { child { anotherCild }} }") `shouldBe`
         GQLQuery (Just "foo") [] [GQLField "parent" [GQLField "child" [GQLField "anotherCild" []]]]
