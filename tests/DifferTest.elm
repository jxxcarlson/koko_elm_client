module DifferTest exposing (..)

import LatexParser.Differ exposing (..)


-- http://package.elm-lang.org/packages/elm-community/elm-test/latest

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)


p1 =
    [ "red", "green", "blue" ]


p2 =
    [ "red", "verde", "blue" ]


r1 =
    List.map String.toUpper p1


suite : Test
suite =
    describe "Differ"
        -- Nest as many descriptions as you like.
        [ test "(1) compute diff of p1 and p2 " <|
            \_ ->
                let
                    diffRecord =
                        diff p1 p2
                in
                    Expect.equal diffRecord.middleSegmentInTarget [ "verde" ]
        , test "(2) compute renderDiff for diff p1 p2 and r1" <|
            \_ ->
                let
                    diffRecord =
                        diff p1 p2

                    r2 =
                        renderDiff String.toUpper diffRecord r1
                in
                    Expect.equal r2 [ "RED", "VERDE", "BLUE" ]
        ]
