module LatexParserTest exposing (..)

import LatexParser.Latex exposing (..)
import LatexParser.Parser exposing (..)
import LatexParser.Render exposing (transformText)
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
            , test "(LL2) parses a multiline input" <|
                \_ ->
                    let
                        result =
                            run latexList "An equation: $\\alpha^2 + \\beta^2$\n\na b \\emph{test.} \\begin{theorem} This is true: $a^n = 1$ has $n$ solutions. \\end{theorem}  \n\n% (2) \n\nPythagoras: $ a^2 + b^2 = c^2$\n\n\nNewton: $$ \\int_0^1 x^n dx = \\frac{1}{n+1} $$\n\n"
                    in
                        case (result) of
                            Ok v ->
                                Expect.equal v { value = [ Word "An", Word "equation:", InlineMath { value = "\\alpha^2 + \\beta^2" }, Word "a", Word "b", Macro { name = "emph", args = [ "test." ] }, Environment { env = "theorem", body = " This is true: $a^n = 1$ has $n$ solutions. " }, Comment (), Word "Pythagoras:", InlineMath { value = " a^2 + b^2 = c^2" }, Word "Newton:", DisplayMath { value = " \\int_0^1 x^n dx = \\frac{1}{n+1} " } ] }

                            _ ->
                                Expect.fail "Wrong type"
            ]
        , describe "Render"
            [ test "(R W) renders a sequence of words" <|
                \_ ->
                    let
                        result =
                            run latexList "ho ho ho " |> latexListGet |> List.map LatexParser.Render.transformLatex
                    in
                        Expect.equal result [ "ho", "ho", "ho" ]
            , test "(R C) renders commments -- i.e., doesn't show them." <|
                \_ ->
                    let
                        input =
                            "\\emph{foo} bar: $a^2 + b^2 = c^2$ \\begin{theorem} There are infinitely many primes.\\end{theorem} % This is a test.\n"

                        expectedOutput =
                            "<b>foo</b> bar:  $a^2 + b^2 = c^2$  \n<strong>Theorem</strong>\n<it>\n There are infinitely many primes.\n</it>\n "
                    in
                        Expect.equal (LatexParser.Render.transformText input) expectedOutput
            , test "(R Simple) render simple example" <|
                \_ ->
                    let
                        input =
                            """% Test file
\\emph{Pythagoras} said: \\[ a^2 + b^2 = c^2 \\] % basic geometry
Some physics: \\begin{equation}
   E = mc^2
\\end{equation}

"""

                        expectedOutput =
                            " <b>Pythagoras</b> said: \n$$\n a^2 + b^2 = c^2 \n$$\n  Some physics: \n<strong>Equation</strong>\n<it>\n\n   E = mc^2\n\n</it>\n"
                    in
                        Expect.equal (LatexParser.Render.transformText input) expectedOutput
            ]

        -- end describe Render
        ]
