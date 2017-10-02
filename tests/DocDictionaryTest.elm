module DocDictionaryTest exposing (..)

import Document.Document as Document exposing (defaultDocument)
import Document.Dictionary as D exposing (..)
import Dict
import Types exposing (Document)


-- http://package.elm-lang.org/packages/elm-community/elm-test/latest

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)


suite : Test
suite =
    describe "Document Dictionary"
        -- Nest as many descriptions as you like.
        [ test "(1) set up dictionary" <|
            \_ ->
                let
                    dict =
                        D.empty
                in
                    Expect.equal (Dict.size dict) 0
        , test "(2) insert a document" <|
            \_ ->
                let
                    dict =
                        D.empty

                    dict2 =
                        D.insert "foo" Document.defaultDocument dict
                in
                    Expect.equal (Dict.size dict2) 1
        , test "(3) remove a document" <|
            \_ ->
                let
                    dict =
                        D.empty

                    dict2 =
                        D.insert "foo" Document.defaultDocument dict

                    dict3 =
                        D.remove "foo" dict
                in
                    Expect.equal (Dict.size dict3) 0
        , test "(4) test for membership (key present)" <|
            \_ ->
                let
                    dict =
                        D.empty

                    dict2 =
                        D.insert "foo" Document.defaultDocument dict
                in
                    Expect.equal (D.member "foo" dict2)
                        True
        , test "(5) test for membership (key not present)" <|
            \_ ->
                let
                    dict =
                        D.empty

                    dict2 =
                        D.insert "foo" Document.defaultDocument dict
                in
                    Expect.equal (D.member "bar" dict2) False
        ]
