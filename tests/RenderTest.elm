module RenderTest exposing (..)

import MiniLatex.Parser exposing (..)
import MiniLatex.Render exposing (..)
import Parser exposing (run)


-- http://package.elm-lang.org/packages/elm-community/elm-test/latest

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)


suite : Test
suite =
    describe "MiniLatex Parser"
        -- Nest as many descriptions as you like.
        [ test "(1) Words (plain text)" <|
            \_ ->
                let
                    renderOutput =
                        renderString parse "This is a test."

                    expectedOutput =
                        "This is a test."
                in
                    Expect.equal renderOutput expectedOutput
        , test "(2) Comment" <|
            \_ ->
                let
                    renderOutput =
                        renderString parse "% This is a comment\n"

                    expectedOutput =
                        ""
                in
                    Expect.equal renderOutput expectedOutput
        , test "(3.1) InlineMath" <|
            \_ ->
                let
                    renderOutput =
                        renderString parse "$a^2 = 7$"

                    expectedOutput =
                        "$a^2 = 7$"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(3.2) DisplayMath" <|
            \_ ->
                let
                    renderOutput =
                        renderString parse "$$b^2 = 3$$"

                    expectedOutput =
                        "$$b^2 = 3$$"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(3.3) DisplayMath (Brackets)" <|
            \_ ->
                let
                    renderOutput =
                        renderString parse "\\[b^2 = 3\\]"

                    expectedOutput =
                        "$$b^2 = 3$$"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(4.1) parse words and unimplemented macros" <|
            \_ ->
                let
                    renderOutput =
                        Debug.log "RENDERED OUTPUT"
                            renderString
                            parse
                            "a b c \\foo \\bar{1} \\baz{1}{2}"

                    expectedOutput =
                        "a b c"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(4.2) parse \\italic{a b c}" <|
            \_ ->
                let
                    renderOutput =
                        Debug.log "RENDERED OUTPUT"
                            renderString
                            parse
                            "\\italic{a b c}"

                    expectedOutput =
                        "<it>a b c</it>"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(4.3) parse \\italic{a b $c==d$}" <|
            \_ ->
                let
                    renderOutput =
                        renderString
                            parse
                            "\\italic{a b $c==d$}"

                    expectedOutput =
                        "<it>a b $c==d$</it>"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(5.1) Environment" <|
            \_ ->
                let
                    renderOutput =
                        renderString
                            parse
                            "\\begin{theorem}\nInfinity is \\emph{very} large: $\\infinity^2 = \\infinity$. \\end{theorem}"

                    expectedOutput =
                        "\n<div class=\"environment\">\n<strong>Theorem 0</strong>\n<div class=\"italic\">\nInfinity is <it>very</it> large: $\\infinity^2 = \\infinity$ .\n</div>\n</div>\n"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(5.2) Nested Environment" <|
            \_ ->
                let
                    renderOutput =
                        renderString
                            parse
                            "\\begin{theorem}\n\\begin{a}x y z\\end{a}\\end{theorem}"

                    expectedOutput =
                        "\n<div class=\"environment\">\n<strong>Theorem 0</strong>\n<div class=\"italic\">\n\n<div class=\"environment\">\n<strong>A</strong>\n<div class=\"italic\">\nx y z\n</div>\n</div>\n\n</div>\n</div>\n"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(5.3) Equation Environment" <|
            \_ ->
                let
                    renderOutput =
                        renderString
                            parse
                            "\\begin{equation}\n\\int_0^1 x^n dx = \\frac{1}{n+1}\n\\end{equation}"

                    expectedOutput =
                        "\n$$\n\\begin{equation}\n\\int_0^1 x^n dx = \\frac{1}{n+1}\n\\end{equation}\n$$\n"
                in
                    Expect.equal renderOutput expectedOutput
        ]
