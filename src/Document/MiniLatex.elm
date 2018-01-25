module Document.MiniLatex exposing (macros)

import Document.Dictionary as Dictionary
import Regex
import Types exposing (DocumentDict)


macros : DocumentDict -> String
macros documentDict =
    if Dictionary.member "texmacros" documentDict then
        Dictionary.getContent "texmacros" documentDict
            |> Regex.replace Regex.All (Regex.regex "\n+") (\_ -> "\n")
    else
        ""
