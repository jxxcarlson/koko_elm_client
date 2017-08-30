module Document.Preprocess exposing(preprocess, preprocessSource)
-- module Document.Preprocess exposing(preprocess, preprocessSource)

import Regex exposing(..)
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
  source
    |> transformXLinks



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
          content2 = content
            |> LatexParser.Render.transformText
            |> transformXLinks
        in
          content
{-|
 xlink::public/123[label] => http://www.knode.io##public/123[Labe]
 xlink::public/123[label] => URL##public/123[Label]
 Example : http://www.knode.io##public/113[Python notes]
-}
transformXLinks : String -> String
transformXLinks source =
    String.Extra.replace "xlink::" (Configuration.client ++ "##document/") source

-- http://www.knode.io/##public/113


findImages1 : String -> List String
findImages1 str =
    let
      -- rx = (regex "\\s(http://.*jpg)\\s")
      rx = (regex "\\s((https|http)://\\S*?(jpg|png))\\s")
    in
      Regex.find All rx str
      |> List.map .submatches |> List.concat |> List.map (Maybe.withDefault "")

preprocessSource : String -> String
preprocessSource source =
  source |> transformImages

findImages : String -> List String
findImages str =
    let
      rx = (regex "\\s((https|http)://\\S*?(jpg|jpeg|png))\\s")
    in
      Regex.find All rx str
      |> List.map .submatches
      |> List.map List.head
      |> List.map (Maybe.withDefault (Just ""))
      |> List.map (Maybe.withDefault "")


transformImage : String -> String -> String
transformImage imageString sourceString =
  let
    imageRef = "image::" ++ imageString ++ "[]"
  in
    String.Extra.replace imageString imageRef sourceString

transformImages : String -> String
transformImages source =
  let
    imageList = findImages source
  in
    List.foldr transformImage source imageList

-- THIS WORKS: PP.findImage str2 |> List.head |> Maybe.map .submatches |> Maybe.withDefault [] |> List.filterMap identity

-- > PP.findImage str2 |> List.head |> Maybe.andThen (\x -> List.head x.submatches) |> Maybe.map (Maybe.withDefault "")
-- Just "http://zoo.org/bird.jpg" : Maybe.Maybe String


--   PP.findImage str |> List.head |> Maybe.andThen (\m -> Just m.submatches) |> Maybe.andThen List.head
-- Just (Just "http://d2fbmjy3x0sdua.cloudfront.net/cdn/farfuture/ultnKZor9cPFMcyALjvFJEFrjJhxsr_-ljICzfTVqWA/mtime:1486670068/sites/default/files/styles/nas_bird_teaser_illustration/public/4492_Sibl_9780307957900_art_r1.jpg")
--     : Maybe.Maybe (Maybe.Maybe String)
