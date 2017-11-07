module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import MiniLatex.Differ as Differ exposing (diff, renderDiff, paragraphify)
import Data exposing (qftIntroText, qftIntroText2)
import MiniLatex.Render as Render
import MiniLatex.LatexState as LatexState exposing (emptyLatexState)


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
        , benchmark3 "renderDiff, one substitution" renderDiff (Render.transformText emptyLatexState) diffRecord r1
        , benchmark2 "List.map Render.transformText" List.map (Render.transformText emptyLatexState) p1
        , benchmark1 "initialize differ" setup qftIntroText
        , benchmark2 "cycle differ" cycle qftIntroText qftIntroText2
        , benchmark2 "double cycle differ" doubleCycle qftIntroText qftIntroText2
        ]


p1 =
    Differ.paragraphify qftIntroText


p2 =
    Differ.paragraphify qftIntroText2


r1 =
    List.map (Render.transformText emptyLatexState) p1


diffRecord =
    diff p1 p2


setup text =
    Differ.initialize (Render.transformText emptyLatexState) text


cycle text1 text2 =
    let
        editorRecord =
            Differ.initialize (Render.transformText emptyLatexState) text1
    in
        Differ.update (Render.transformText emptyLatexState) editorRecord text2


doubleCycle text1 text2 =
    let
        editorRecord =
            Differ.initialize (Render.transformText emptyLatexState) text1
    in
        cycle2 text1 text2 editorRecord


cycle2 text1 text2 editorRecord =
    let
        editorRecord2 =
            -- update : (String -> String) -> EditRecord -> String -> EditRecord
            Differ.update (Render.transformText emptyLatexState) editorRecord text2
    in
        Differ.update (Render.transformText emptyLatexState) editorRecord2 text1



-- r2 =
--     renderDiff String.toUpper diffRecord r1
