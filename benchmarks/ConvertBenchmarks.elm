module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Regex
import String.Extra
import Dict


{-
   http://package.elm-lang.org/packages/BrianHicks/elm-benchmark/latest

-}


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe ""
        [ -- describe "Parse"
          -- nest as many descriptions as you like
          -- [ -- benchmark1 "qftIntroText" LatexParser.Paragraph.formatDocument qftIntroText
          --   benchmark1 "convert" (convert mapping2) testString2
          -- , benchmark1 "convert2" convert2 testString2
          -- ]
          Benchmark.compare
            "initialize"
            -- compare the results of two benchmarks
            (benchmark1 "convert" (convert mapping6) testString4)
            (benchmark1 "convert2" convert6 testString4)
        ]


{-| <https://ellie-app.com/8JGHb3gxGa1/1>


## Trial convert convertNaive Delta Terms Length

1 34.6 37.2 7.6% 2 3300
2 42.2 78.3 85.0% 4 6100
3 70.0 109.4 56.3% 6 9600


## 4 82.0 153.1 86.6% 8 13000

-}
convert mapping str =
    Dict.foldr
        (\key val acc ->
            Regex.replace
                Regex.All
                (Regex.regex ("\\" ++ key))
                (\_ -> val)
                acc
        )
        str
        mapping


mapping2 =
    Dict.fromList [ ( "--", "-" ), ( "---", "-" ) ]


mapping4 =
    Dict.fromList [ ( "--", "-" ), ( "---", "-" ), ( "red", "rojo" ), ( "blue", "azul" ) ]


mapping6 =
    Dict.fromList [ ( "--", "-" ), ( "---", "-" ), ( "red", "rojo" ), ( "blue", "azul" ), ( "black", "negro" ), ( "white", "blanco" ) ]


mapping8 =
    Dict.fromList [ ( "--", "-" ), ( "---", "-" ), ( "red", "rojo" ), ( "blue", "azul" ), ( "black", "negro" ), ( "white", "blanco" ), ( "flower", "flor" ), ( "tree", "arbol" ) ]


convert2 str =
    str
        |> String.Extra.replace "--" "–"
        |> String.Extra.replace "---" "—"


convert4 str =
    str
        |> String.Extra.replace "--" "–"
        |> String.Extra.replace "---" "—"
        |> String.Extra.replace "red" "rojo"
        |> String.Extra.replace "blue" "azul"


convert6 str =
    str
        |> String.Extra.replace "--" "–"
        |> String.Extra.replace "---" "—"
        |> String.Extra.replace "red" "rojo"
        |> String.Extra.replace "blue" "azul"
        |> String.Extra.replace "black" "negro"
        |> String.Extra.replace "white" "blanco"


convert8 str =
    str
        |> String.Extra.replace "--" "–"
        |> String.Extra.replace "---" "—"
        |> String.Extra.replace "red" "rojo"
        |> String.Extra.replace "blue" "azul"
        |> String.Extra.replace "black" "negro"
        |> String.Extra.replace "white" "blanco"
        |> String.Extra.replace "flower" "flor"
        |> String.Extra.replace "tree" "arbol"


testString1 =
    "Ho ho ho -- ha ha ha --- ho ho ho"


testString2 =
    String.repeat 100 "Ho ho ho -- ha ha ha --- ho ho ho"


testString3 =
    String.repeat 100 "Ho ho ho -- ha ha ha --- ho ho ho red ha ha ha blue, ho ho ho"


testString4 =
    String.repeat 100 "Ho ho ho -- ha ha ha --- ho ho ho red ha ha ha blue, ho ho ho,  ho ho ho black, ho ho ho, white "


testString5 =
    String.repeat 100
        "Ho ho ho -- ha ha ha --- ho ho ho red ha ha ha blue, ho ho ho black, ho ho ho, white , ho ho ho flower, ho ho ho, tree blah, blah "
