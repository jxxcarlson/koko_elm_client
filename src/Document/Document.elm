module Document.Document exposing (..)

import Types exposing (Document)
import Regex


hasTag : String -> Document -> Bool
hasTag tagg document =
    List.any (\x -> x == tagg) document.tags


equationRegexString =
    "\\[env\\.equation\\]\n--\n(.+?)\n--\n"


testString =
    "foo\n[env.equation]\n--\nla di dah\n--\nbar\n[env.equation]\n--\nho ho ho\n--\nha ha ha\n"



-- replaceExpression : String -> String -> String


findMatches regex text =
    text
        |> Regex.find Regex.All (Regex.regex regex)



-- [{ match = "::foo::", submatches = [Just "foo"], index = 9, number = 1 }]
