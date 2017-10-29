module AccumulatorTest exposing (..)

import LatexParser.Accumulator exposing (..)
import LatexParser.Render as Render
import Document.Differ as Differ
import Data
import Dict


-- http://package.elm-lang.org/packages/elm-community/elm-test/latest

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)


suite : Test
suite =
    describe "XXX"
        -- Nest as many descriptions as you like.
        [ test "(1) lngth of input" <|
            \_ ->
                Expect.equal (Data.qftIntroText |> String.length) 18816
        , test "(2) parse into a list of Latex elements" <|
            \_ ->
                let
                    parseData1 =
                        Data.qftIntroText
                            |> Differ.paragraphify
                            |> List.map Render.parseParagraph
                in
                    Expect.equal (List.length parseData1) 93
        , test "(3) parse and render paragraphs, verify final LatexState" <|
            \_ ->
                let
                    data =
                        Data.qftIntroText
                            |> Differ.paragraphify
                            |> accumulator Render.parseParagraph renderParagraph updateState

                    d =
                        Dict.fromList [ ( "1", "4.14" ), ( "GE", "4.16" ), ( "\\hbar", "3.3" ), ( "\\sin \\theta_1", "4.21" ), ( "\\sin\\theta_2", "4.17" ), ( "d{\\bf p", "2.2" ), ( "h\\nu", "4.20" ) ]

                    expectedOutput =
                        { s1 = 4, s2 = 4, s3 = 0, tno = 0, eqno = 21, dict = d }
                in
                    Expect.equal (Tuple.second data) expectedOutput
        , test "(4) check that accumulator produces a list of the correct length" <|
            \_ ->
                let
                    data =
                        Data.qftIntroText
                            |> Differ.paragraphify
                            |> accumulator Render.parseParagraph renderParagraph updateState
                in
                    Expect.equal (Tuple.first data |> List.length) 93
        , test "(5) transformText" <|
            \_ ->
                let
                    data =
                        Data.qftIntroText
                            |> Differ.paragraphify
                            |> transformParagraphs
                in
                    Expect.equal (data |> List.length) 93
        , test "(6) check that '1 Introduction' exists as first section in output" <|
            \_ ->
                let
                    data =
                        Data.qftIntroText
                            |> Differ.paragraphify
                            |> transformParagraphs
                in
                    let
                        firstString =
                            data |> List.head |> Maybe.withDefault ""
                    in
                        Expect.equal (String.contains "1 Introduction" firstString) True
        ]
