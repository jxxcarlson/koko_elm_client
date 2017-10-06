module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Document.Differ as Differ exposing (diff, renderDiff, paragraphify)
import LatexParser.TextSample exposing (qftIntroText1, qftIntroText2)
import LatexParser.Render as Render


{-
   http://package.elm-lang.org/packages/BrianHicks/elm-benchmark/latest

-}


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "Differ"
        [ benchmark2 "diff, identical sequences" diff p1 p1
        , benchmark2 "diff, one substitution" diff p1 p2
        , benchmark3 "renderDiff, one substitution" renderDiff Render.transformText diffRecord r1
        , benchmark2 "List.map Render.transformText" List.map Render.transformText p1
        , benchmark1 "initialize differ" setup qftIntroText1
        , benchmark2 "cycle differ" cycle qftIntroText1 qftIntroText2
        , benchmark2 "double cycle differ" doubleCycle qftIntroText1 qftIntroText2
        ]


p1 =
    Differ.paragraphify qftIntroText1


p2 =
    Differ.paragraphify qftIntroText2


r1 =
    List.map Render.transformText p1


diffRecord =
    diff p1 p2


setup text =
    Differ.initialize Render.transformText text


cycle text1 text2 =
    let
        editorRecord =
            Differ.initialize Render.transformText text1
    in
        Differ.update Render.transformText text2 editorRecord


doubleCycle text1 text2 =
    let
        editorRecord =
            Differ.initialize Render.transformText text1
    in
        cycle2 text1 text2 editorRecord


cycle2 text1 text2 editorRecord =
    let
        editorRecord2 =
            Differ.update Render.transformText text2 editorRecord
    in
        Differ.update Render.transformText text1 editorRecord2



-- r2 =
--     renderDiff String.toUpper diffRecord r1
