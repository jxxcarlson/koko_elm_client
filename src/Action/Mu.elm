module Mu exposing (render)


render : String -> String
render input =
    input
        |> paragraphs
        |> List.map applyTag ("p")


paragraphs : String -> List String
paragraphs input =
    input
        |> String.split "\n\n"
        |> List.filter (\a -> a /= "")


applyTag : String -> String -> String
applyTag tag input =
    "\n<" ++ tag ++ ">\n" ++ input ++ "\n</" ++ tag ++ ">\n"
