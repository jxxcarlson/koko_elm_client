module Action.Preprocess exposing(preprocess)
import Regex
import LatexParser.Render
import Types exposing(Document)
import Configuration
import String.Extra



preprocess : String -> Document -> String
preprocess content document =
    if document.attributes.docType == "master" then
        preprocessMaster content
    else if document.attributes.textType == "latex" then
        preprocessLatex content
    else
        basicPreprocess content

basicPreprocess : String -> String
basicPreprocess source =
  source |> transformXLinks


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
          content2 = content
            |> LatexParser.Render.transformText
            |> transformXLinks
          _ = Debug.log "content2" content2
        in
          content
{-|
 xlink::public/123[label] => http://www.knode.io##public/123[Labe]
 xlink::public/123[label] => URL##public/123[Label]
 Example : http://www.knode.io##public/113[Python notes]
-}
transformXLinks : String -> String
transformXLinks source =
    String.Extra.replace "xlink::" (Configuration.client ++ "##") source
