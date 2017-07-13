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
            [ test "parses macros with one argument and trailing space" <|
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
            , test "parses macros with one argument and trailing \n" <|
                \_ ->
                    let
                        result =
                            run latex "\\emph{foo}\n" |> latexGet
                    in
                        case (result) of
                            Macro _ ->
                                Expect.equal result (Macro { name = "emph", args = [ "foo" ] })

                            _ ->
                                Expect.fail "Wrong type"
            , test "parses macros with one argument and no trailing character" <|
                \_ ->
                    let
                        result =
                            run latex "\\emph{foo}" |> latexGet
                    in
                        case (result) of
                            Macro _ ->
                                Expect.equal result (Macro { name = "emph", args = [ "foo" ] })

                            _ ->
                                Expect.fail "Wrong type"
            , test "parses macros with one argument (2)" <|
                \_ ->
                    let
                        result =
                            run latex "\\emph{foo} " |> latexGet
                    in
                        case (result) of
                            Macro _ ->
                                Expect.equal result (Macro { name = "emph", args = [ "foo" ] })

                            _ ->
                                Expect.fail "Wrong type"
            , test "parses words with trailing space and \n" <|
                \_ ->
                    let
                        result =
                            run latex "a b c \n" |> latexGet
                    in
                        case (result) of
                            Words _ ->
                                Expect.equal result (Words { value = [ "a", "b", "c" ] })

                            _ ->
                                Expect.fail "Wrong type"
            , test "parses words with trailing \n" <|
                \_ ->
                    let
                        result =
                            run latex "a b c\n" |> latexGet
                    in
                        case (result) of
                            Words _ ->
                                Expect.equal result (Words { value = [ "a", "b", "c" ] })

                            _ ->
                                Expect.fail "Wrong type"
            , test "parses environments with trailing \n" <|
                \_ ->
                    let
                        result =
                            run latex "\\begin{foo}bar\\end{foo}\n" |> latexGet
                    in
                        case (result) of
                            Environment _ ->
                                Expect.equal result (Environment { env = "foo", body = "bar" })

                            _ ->
                                Expect.fail "Wrong type"
            , test "parses inlineMath with trailing space" <|
                \_ ->
                    let
                        result =
                            run latex "$a^2 = 1$ " |> latexGet
                    in
                        case (result) of
                            InlineMath _ ->
                                Expect.equal result (InlineMath { value = "a^2 = 1" })

                            _ ->
                                Expect.fail "Wrong type"
            , test "parses displayMath with trailing \n" <|
                \_ ->
                    let
                        result =
                            run latex "$$a^2 = 1$$\n" |> latexGet
                    in
                        case (result) of
                            DisplayMath _ ->
                                Expect.equal result (DisplayMath { value = "a^2 = 1" })

                            _ ->
                                Expect.fail "Wrong type"
            ]
        , describe
            "LatexList"
            -- Nest as many descriptions as you like.
            [ test "parses one macro with no trailing space" <|
                \_ ->
                    let
                        result =
                            run latexList "\\emph{foo}"
                    in
                        case (result) of
                            Ok v ->
                                Expect.equal v { value = [ Macro { name = "emph", args = [ "foo" ] } ] }

                            _ ->
                                Expect.fail "Wrong type"
            , test "parses two macros with no trailing space" <|
                \_ ->
                    let
                        result =
                            run latexList "\\emph{foo} \\yada{1}{2}"
                    in
                        case (result) of
                            Ok v ->
                                Expect.equal v { value = [ Macro { name = "emph", args = [ "foo" ] }, Macro { name = "yada", args = [ "1", "2" ] } ] }

                            _ ->
                                Expect.fail "Wrong type"
            , test "parses text with words and macros" <|
                \_ ->
                    let
                        result =
                            run latexList "ho ho ho \\emph{foo} ha ha ha \\yada{1}{2}\n"
                    in
                        case (result) of
                            Ok v ->
                                Expect.equal v { value = [ Words { value = [ "ho", "ho", "ho" ] }, Macro { name = "emph", args = [ "foo" ] }, Words { value = [ "ha", "ha", "ha" ] }, Macro { name = "foo", args = [ "1", "2" ] } ] }

                            _ ->
                                Expect.fail "Wrong type"
            ]
        ]
