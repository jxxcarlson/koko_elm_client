module RenderTest exposing (..)

import MiniLatex.Parser exposing (..)
import MiniLatex.Render exposing (..)
import Parser exposing (run)


-- http://package.elm-lang.org/packages/elm-community/elm-test/latest

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)


renderHelper parser str =
    let
        parserOutput =
            run parse str

        renderOutput =
            case parserOutput of
                Ok latexExpression ->
                    render emptyLatexState latexExpression

                Err _ ->
                    "PARSE ERROR"
    in
        renderOutput


suite : Test
suite =
    describe "MiniLatex Parser"
        -- Nest as many descriptions as you like.
        [ test "(0) Words (plain text)" <|
            \_ ->
                let
                    renderOutput =
                        renderHelper parse "This is a test."

                    expectedOutput =
                        "This is a test."
                in
                    Expect.equal renderOutput expectedOutput
        , test "(1) Comment" <|
            \_ ->
                let
                    renderOutput =
                        renderHelper parse "% This is a comment\n"

                    expectedOutput =
                        ""
                in
                    Expect.equal renderOutput expectedOutput
        , test "(2) InlineMath" <|
            \_ ->
                let
                    renderOutput =
                        renderHelper parse "$a^2 = 7$"

                    expectedOutput =
                        "$a^2 = 7$"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(3) DisplayMath" <|
            \_ ->
                let
                    renderOutput =
                        renderHelper parse "$$b^2 = 3$$"

                    expectedOutput =
                        "$$b^2 = 3$$"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(4) DisplayMath (Brackets)" <|
            \_ ->
                let
                    renderOutput =
                        renderHelper parse "\\[b^2 = 3\\]"

                    expectedOutput =
                        "$$b^2 = 3$$"
                in
                    Expect.equal renderOutput expectedOutput
        , test "(5) latexList words and macros" <|
            \_ ->
                let
                    parsedInput =
                        run latexList "a b \\foo \\bar{1} \\baz{1}{2}"

                    expectedOutput =
                        Ok (LatexList ([ LXString "a b", Macro "foo" [], Macro "bar" [ "1" ], Macro "baz" [ "1", "2" ] ]))
                in
                    Expect.equal parsedInput expectedOutput
        , test "(6) Environment" <|
            \_ ->
                let
                    parsedInput =
                        run parse "\\begin{theorem}\nInfinity is \\emph{very} large: $\\infinity^2 = \\infinity$. \\end{theorem}"

                    expectedOutput =
                        Ok
                            (Environment "theorem"
                                (LatexList
                                    ([ LXString "Infinity is"
                                     , Macro "emph" [ "very" ]
                                     , LXString "large:"
                                     , InlineMath "\\infinity^2 = \\infinity"
                                     , LXString "."
                                     ]
                                    )
                                )
                            )
                in
                    Expect.equal parsedInput expectedOutput
        , test "(7) Nested Environment" <|
            \_ ->
                let
                    parsedInput =
                        run parse " \\begin{th}  \\begin{a}$$hahah$$\\begin{x}yy\\end{x}\\end{a}\\begin{a} a{1}{2} b c yoko{1} $foo$ yada $$bar$$ a b c \\begin{u}yy\\end{u} \\end{a}\n\\end{th}"

                    expectedOutput =
                        Ok
                            (Environment "th"
                                (LatexList
                                    ([ Environment "a" (LatexList ([ DisplayMath "hahah", Environment "x" (LatexList ([ LXString "yy" ])) ]))
                                     , Environment "a"
                                        (LatexList
                                            ([ LXString "a{1}{2} b c yoko{1}"
                                             , InlineMath "foo"
                                             , LXString "yada"
                                             , DisplayMath "bar"
                                             , LXString "a b c"
                                             , Environment "u" (LatexList ([ LXString "yy" ]))
                                             ]
                                            )
                                        )
                                     ]
                                    )
                                )
                            )
                in
                    Expect.equal parsedInput expectedOutput
        , test "(8) Itemized List" <|
            \_ ->
                let
                    parsedInput =
                        run latexList "\\begin{itemize} \\item aaa.\n \\item bbb.\n \\itemitem xx\n\\end{itemize}"

                    expectedOutput =
                        Ok
                            (LatexList
                                ([ Environment "itemize"
                                    (LatexList
                                        ([ Item 1 (LatexList ([ LXString "aaa." ]))
                                         , Item 1 (LatexList ([ LXString "bbb." ]))
                                         , Item 2 (LatexList ([ LXString "xx" ]))
                                         ]
                                        )
                                    )
                                 ]
                                )
                            )
                in
                    Expect.equal parsedInput expectedOutput
        ]
