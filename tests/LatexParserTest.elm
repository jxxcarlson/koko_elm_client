module LatexParserTest exposing (..)

import LatexParser.Parser exposing (..)
import LatexParser.Latex exposing (..)
import LatexParser.Render exposing (transform)
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
            [ test "(M1) parses macros with one argument and trailing space" <|
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
            , test "(M2) parses macros with one argument and trailing EOL" <|
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
            , test "(M3) parses macros with one argument and no trailing character" <|
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
            , test "(M4) parses macros with one argument (2)" <|
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
            , test "(E1) parses environments with trailing EOL" <|
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
            , test "(IM) parses inlineMath with trailing space" <|
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
            , test "(DM) parses displayMath with trailing EOL" <|
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
            [ test "(LL M1) parses one macro with no trailing space" <|
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
            , test "(LL M2) parses two macros with no trailing space" <|
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
            , test "(LL WM) parses text with words and macros" <|
                \_ ->
                    let
                        result =
                            run latexList "ho ho ho \\emph{foo} ha ha ha \\yada{1}{2}\n"
                    in
                        case (result) of
                            Ok v ->
                                Expect.equal v { value = [ Word "ho", Word "ho", Word "ho", Macro { name = "emph", args = [ "foo" ] }, Word "ha", Word "ha", Word "ha", Macro { name = "yada", args = [ "1", "2" ] } ] }

                            _ ->
                                Expect.fail "Wrong type"
            ]
        , describe "Render"
            [ test "(R W) renders a sequence of words" <|
                \_ ->
                    let
                        result =
                            run latexList "ho ho ho " |> latexListGet |> List.map LatexParser.Render.transform
                    in
                        Expect.equal result [ "ho", "ho", "ho" ]
            ]
        ]
