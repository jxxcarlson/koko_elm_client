module LatexParserTest exposing (..)

import LatexParser.Parser exposing (..)
import LatexParser.Latex exposing (..)
import Parser exposing (run)


-- http://package.elm-lang.org/packages/elm-community/elm-test/latest

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Latex Parser"
        [ describe "latex"
            -- Nest as many descriptions as you like.
            [ test "parses macros with one argument" <|
                \_ ->
                    let
                        result =
                            run latex "\\emph{foo} " |> latexGet
                    in
                        case (result) of
                            Macro v ->
                                Expect.equal v.name "emph"

                            _ ->
                                Expect.fail "Wrong type"
            , test "parses macros with one argument (2)" <|
                \_ ->
                    let
                        result =
                            run latex "\\emph{foo} " |> latexGet
                    in
                        case (result) of
                            Macro v ->
                                Expect.equal result (Macro { name = "emph", args = [ "foo" ] })

                            _ ->
                                Expect.fail "Wrong type"
            ]
        ]
