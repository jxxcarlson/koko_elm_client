module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Document.Preprocess
import MiniLatex.Paragraph
import MiniLatex.Parser exposing (Latex(..), latex, latexParser, latexListGet)
import MiniLatex.Render exposing (transformText)
import Parser
import Regex
import String.Extra
import Dict
import Data


{-
   http://package.elm-lang.org/packages/BrianHicks/elm-benchmark/latest

-}


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe ""
        [ describe "Parse"
            [
            , benchmark1 "formatDocument" LatexParser.Paragraph.formatDocument qftIntroText
            , benchmark1 "parseDocument" LatexParser.Paragraph.parseDocument qftIntroText
            , benchmark1 "replaceStrings" LatexParser.Paragraph.replaceStrings qftIntroText

            -- , benchmark "latexParser" (Parser.run latexParser qftIntroText)
            , benchmark1 "transformText" transformText qftIntroText
            , benchmark1 "formatParagraphList" LatexParser.Paragraph.formatParagraphList (List.repeat 3 [ line1 ])
            ]

        -- Benchmark.compare
        -- "initialize"
        -- -- compare the results of two benchmarks
        -- (benchmark1 "convert" (convert mapping6) testString4)
        -- (benchmark1 "convert2" convert6 testString4)
        ]


{-| 400 chars
-}
line1 =
    "From the trajectory are defined the velocity ${\\bf v}(t) = d{\\bf r}(t)/dt$ and the acceleration ${\\bf a}(t) = d{\\bf v}(t)/dt$.  And once these are given, we have Newton's second law, ${\\bf F} = m{\\bf a}$, where a force ${\\bf F}$ is applied to a body of mass $m$. To set the stage for later developments, recall that the momentum of a body is ${\\bf p} = m{\\bf v}$, so that Newton's law can be written\n"


qftIntroText : String
qftIntroText = Data.qftIntroText
