module Action.Preprocess exposing(preprocess)
import Regex
import LatexParser.Render
import Types exposing(Document)


preprocess : String -> Document -> String
preprocess content document =
    if document.attributes.docType == "master" then
        preprocessMaster content
    else if document.attributes.textType == "latex" then
        preprocessLatex content
    else
        content


preprocessMaster : String -> String
preprocessMaster content =
    (String.split "TOC:\n" content) |> List.head |> Maybe.withDefault ""


replace : String -> String -> String -> String
replace search substitution string =
    string
        |> Regex.replace Regex.All (Regex.regex (Regex.escape search)) (\_ -> substitution)


preprocessLatex : String -> String
preprocessLatex content =
        let
          _ = Debug.log "content" content
          content2 = content |> LatexParser.Render.transformText
          _ = Debug.log "content2" content2
        in
          content2
