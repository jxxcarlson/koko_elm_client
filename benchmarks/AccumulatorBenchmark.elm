module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import MiniLatex.LatexDiffer exposing (initialize, initialize1, initialize2)
import Data exposing (qftIntroText, qftIntroText2)
import MiniLatex.LatexState exposing (emptyLatexState)


{-
   http://package.elm-lang.org/packages/BrianHicks/elm-benchmark/latest

-}


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "Differ"
        [ benchmark1 "initialize" initialize qftIntroText
        , benchmark2 "initialize1" initialize1 emptyLatexState qftIntroText
        , benchmark2 "initialize2" initialize2 emptyLatexState qftIntroText
        ]
