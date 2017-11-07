module Experiment.Levenshtein exposing (..)

{-| Adapted from the Haskel code at
<https://swizec.com/blog/levenshtein-distance-in-haskell/swizec/4801>
-}


initialSegment : List String -> List String
initialSegment x =
    let
        n =
            List.length x
    in
        List.take (n - 1) x


lastString : List String -> String
lastString x =
    List.drop ((List.length x) - 1) x |> List.head |> Maybe.withDefault ""


levenshtein : List String -> List String -> Int
levenshtein s1 s2 =
    let
        ls1 =
            List.length s1

        ls2 =
            List.length s2

        d =
            ls2 - ls1
    in
        if ls1 == 0 && ls2 == 0 then
            0
        else if ls1 > ls2 then
            levenshtein s2 s1
        else if ls1 < ls2 then
            d + levenshtein s1 (List.take (ls2 - d) s2)
        else if lastString s1 == lastString s2 then
            levenshtein (initialSegment s1) (initialSegment s2)
        else
            List.minimum
                [ 1 + levenshtein (initialSegment s1) s2
                , 1 + levenshtein s1 (initialSegment s2)
                , 1 + levenshtein (initialSegment s1) (initialSegment s2)
                ]
                |> Maybe.withDefault 0
