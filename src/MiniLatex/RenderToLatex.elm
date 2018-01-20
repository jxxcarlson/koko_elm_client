module MiniLatex.RenderToLatex
    exposing
        ( makeTableOfContents
        , render
        , renderLatexList
        , renderString
        , transformText
        )

import Dict
import List.Extra
import MiniLatex.Configuration as Configuration
import MiniLatex.KeyValueUtilities as KeyValueUtilities
import MiniLatex.LatexState
    exposing
        ( LatexState
        , TocEntry
        , emptyLatexState
        , getCounter
        , getCrossReference
        , getDictionaryItem
        )
import MiniLatex.Parser exposing (LatexExpression(..), defaultLatexList, latexList)
import Parser
import String.Extra
import Utility


transformText : LatexState -> String -> String
transformText latexState text =
    renderString latexList latexState text



-- |> \str -> "\n<p>" ++ str ++ "</p>\n"
{- FUNCTIONS FOR TESTING THINGS -}


getElement : Int -> List LatexExpression -> LatexExpression
getElement k list =
    List.Extra.getAt k list |> Maybe.withDefault (LXString "xxx")


parseString parser str =
    Parser.run parser str


renderString parser latexState str =
    let
        parserOutput =
            Parser.run parser str

        renderOutput =
            case parserOutput of
                Ok latexExpression ->
                    render latexState latexExpression

                Err error ->
                    "Error: " ++ toString error
    in
    renderOutput



{- TYPES AND DEFAULT VALJUES -}


extractList : LatexExpression -> List LatexExpression
extractList latexExpression =
    case latexExpression of
        LatexList a ->
            a

        _ ->
            []


{-| THE MAIN RENDERING FUNCTION
-}
render : LatexState -> LatexExpression -> String
render latexState latexExpression =
    case latexExpression of
        Comment str ->
            renderComment str

        Macro name args ->
            renderMacro latexState name args

        Item level latexExpression ->
            renderItem latexState level latexExpression

        InlineMath str ->
            "$" ++ str ++ "$"

        DisplayMath str ->
            "$$" ++ str ++ "$$"

        Environment name args ->
            renderEnvironment latexState name args

        LatexList args ->
            renderLatexList latexState args

        LXString str ->
            str


renderLatexList : LatexState -> List LatexExpression -> String
renderLatexList latexState args =
    args |> List.map (render latexState) |> joinList



{- joinList : List String -> String
   join a list of strings to make a single string.
   Adjacent strings l and r are joined by either an empty
   string or a spaxel depending on the terminal character
   of l and the leading character of r.  This is operation
   is a matter of style, but it is important.
-}


joinList : List String -> String
joinList stringList =
    let
        start =
            List.head stringList |> Maybe.withDefault ""
    in
    List.foldl joinDatum2String ( "", "" ) stringList |> Tuple.first


joinDatum2String : String -> ( String, String ) -> ( String, String )
joinDatum2String current datum =
    let
        ( acc, previous ) =
            datum
    in
    case joinType previous current of
        NoSpace ->
            ( acc ++ current, current )

        Space ->
            ( acc ++ " " ++ current, current )


type JoinType
    = Space
    | NoSpace


lastChar =
    String.right 1


firstChar =
    String.left 1


joinType : String -> String -> JoinType
joinType l r =
    let
        lastCharLeft =
            lastChar l

        firstCharRight =
            firstChar r
    in
    if l == "" then
        NoSpace
    else if List.member lastCharLeft [ "(" ] then
        NoSpace
    else if List.member firstCharRight [ ")", ".", ",", "?", "!", ";", ":" ] then
        NoSpace
    else
        Space



{- End new code -}


renderArgList : LatexState -> List LatexExpression -> String
renderArgList latexState args =
    args |> List.map (render latexState) |> List.map (\x -> "{" ++ x ++ "}") |> String.join ""


renderItem : LatexState -> Int -> LatexExpression -> String
renderItem latexState level latexExpression =
    "\\item " ++ render latexState latexExpression ++ "\n\n"


renderComment : String -> String
renderComment str =
    "% " ++ str ++ "\n"



{- ENVIROMENTS -}


renderEnvironment : LatexState -> String -> LatexExpression -> String
renderEnvironment latexState name body =
    "\\begin{" ++ name ++ "}\n" ++ render latexState body ++ "\n\\end{" ++ name ++ "}\n\n"



{- MACROS: DISPATCHERS AND HELPERS -}


macroRenderer : String -> (LatexState -> List LatexExpression -> String)
macroRenderer name =
    "\\" ++ name ++ renderArgList emptyLatexState args


renderImage : LatexState -> List LatexExpression -> String
renderImage latexState args =
    let
        url =
            renderArg 0 latexState args

        label =
            renderArg 1 latexState args

        attributeString =
            renderArg 2 latexState args

        imageAttrs =
            parseImageAttributes attributeString
    in
    if imageAttrs.float == "left" then
        div [ imageFloatLeftStyle imageAttrs ] [ img url imageAttrs, "<br>", label ]
    else if imageAttrs.float == "right" then
        div [ imageFloatRightStyle imageAttrs ] [ img url imageAttrs, "<br>", label ]
    else if imageAttrs.align == "center" then
        div [ imageCenterStyle imageAttrs ] [ img url imageAttrs, "<br>", label ]
    else
        "<image src=\"" ++ url ++ "\" " ++ imageAttributes imageAttrs attributeString ++ " >"


renderImageRef : LatexState -> List LatexExpression -> String
renderImageRef latexState args =
    let
        url =
            renderArg 0 latexState args

        imageUrl =
            renderArg 1 latexState args

        attributeString =
            renderArg 2 latexState args

        imageAttrs =
            parseImageAttributes attributeString
    in
    if imageAttrs.float == "left" then
        a url (div [ imageFloatLeftStyle imageAttrs ] [ img imageUrl imageAttrs ])
    else if imageAttrs.float == "right" then
        a url (div [ imageFloatRightStyle imageAttrs ] [ img imageUrl imageAttrs ])
    else if imageAttrs.align == "center" then
        a url (div [ imageCenterStyle imageAttrs ] [ img imageUrl imageAttrs ])
    else
        a url (div [ imageCenterStyle imageAttrs ] [ img imageUrl imageAttrs ])


a url label =
    "<a href=\"" ++ url ++ "\"  target=\"_blank\" >\n" ++ label ++ "\n</a>"


renderItalic : LatexState -> List LatexExpression -> String
renderItalic latexState args =
    " <span class=italic>" ++ renderArg 0 latexState args ++ "</span>"


renderNewCommand : LatexState -> List LatexExpression -> String
renderNewCommand latexState args =
    let
        command =
            renderArg 0 latexState args

        definition =
            renderArg 1 latexState args
    in
    "\\newcommand{" ++ command ++ "}{" ++ definition ++ "}"


renderRef : LatexState -> List LatexExpression -> String
renderRef latexState args =
    let
        key =
            renderArg 0 latexState args
    in
    getCrossReference key latexState


makeId : String -> String -> String
makeId prefix name =
    String.join ":" [ prefix, Utility.compress ":" name ]


idPhrase : String -> String -> String
idPhrase prefix name =
    let
        compressedName =
            name |> String.toLower |> String.Extra.replace " " ":"
    in
    String.join "" [ "id=\"", makeId prefix name, "\"" ]


tag : String -> String -> String -> String
tag tagName tagProperties content =
    String.join "" [ "<", tagName, " ", tagProperties, " ", ">", content, "</", tagName, ">" ]


renderSection : LatexState -> List LatexExpression -> String
renderSection latexState args =
    let
        sectionName =
            renderArg 0 latexState args

        s1 =
            getCounter "s1" latexState

        label =
            if s1 > 0 then
                toString s1 ++ " "
            else
                ""
    in
    tag "h2" (idPhrase "section" sectionName) (label ++ sectionName)


renderSectionStar : LatexState -> List LatexExpression -> String
renderSectionStar latexState args =
    let
        sectionName =
            renderArg 0 latexState args
    in
    tag "h2" (idPhrase "section" sectionName) sectionName


renderStrong : LatexState -> List LatexExpression -> String
renderStrong latexState args =
    " <span class=\"strong\">" ++ renderArg 0 latexState args ++ "</span> "


renderSubheading : LatexState -> List LatexExpression -> String
renderSubheading latexState args =
    "<div class=\"subheading\">" ++ renderArg 0 latexState args ++ "</div>"


renderSubsection : LatexState -> List LatexExpression -> String
renderSubsection latexState args =
    let
        sectionName =
            renderArg 0 latexState args

        s1 =
            getCounter "s1" latexState

        s2 =
            getCounter "s2" latexState

        label =
            if s1 > 0 then
                toString s1 ++ "." ++ toString s2 ++ " "
            else
                ""
    in
    tag "h3" (idPhrase "subsection" sectionName) (label ++ sectionName)


renderSubsectionStar : LatexState -> List LatexExpression -> String
renderSubsectionStar latexState args =
    let
        sectionName =
            renderArg 0 latexState args
    in
    tag "h3" (idPhrase "subsection" sectionName) sectionName


renderSubSubsection : LatexState -> List LatexExpression -> String
renderSubSubsection latexState args =
    let
        sectionName =
            renderArg 0 latexState args

        s1 =
            getCounter "s1" latexState

        s2 =
            getCounter "s2" latexState

        s3 =
            getCounter "s3" latexState

        label =
            if s1 > 0 then
                toString s1 ++ "." ++ toString s2 ++ "." ++ toString s3 ++ " "
            else
                ""
    in
    tag "h4" (idPhrase "subsubsection" sectionName) (label ++ sectionName)


renderSubSubsectionStar : LatexState -> List LatexExpression -> String
renderSubSubsectionStar latexState args =
    let
        sectionName =
            renderArg 0 latexState args
    in
    tag "h4" (idPhrase "subsubsection" sectionName) sectionName


renderTerm : LatexState -> List LatexExpression -> String
renderTerm latexState args =
    let
        arg =
            renderArg 0 latexState args
    in
    " <span class=italic>" ++ arg ++ "</span>"


renderXLink : LatexState -> List LatexExpression -> String
renderXLink latexState args =
    let
        id =
            renderArg 0 latexState args

        label =
            renderArg 1 latexState args
    in
    " <a href=\"" ++ Configuration.client ++ "##document/" ++ id ++ "\">" ++ label ++ "</a>"


renderXLinkPublic : LatexState -> List LatexExpression -> String
renderXLinkPublic latexState args =
    let
        id =
            renderArg 0 latexState args

        label =
            renderArg 1 latexState args
    in
    " <a href=\"" ++ Configuration.client ++ "##public/" ++ id ++ "\">" ++ label ++ "</a>"



{- TABLE OF CONTENTS -}


renderTitle : LatexState -> List LatexExpression -> String
renderTitle latexState list =
    let
        title =
            getDictionaryItem "title" latexState

        author =
            getDictionaryItem "author" latexState

        date =
            getDictionaryItem "date" latexState

        email =
            getDictionaryItem "email" latexState

        revision =
            getDictionaryItem "revision" latexState

        revisionText =
            if revision /= "" then
                "Last revised " ++ revision
            else
                ""

        titlePart =
            "\n<div class=\"title\">" ++ title ++ "</div>"

        bodyParts =
            [ "<div class=\"authorinfo\">", author, email, date, revisionText, "</div>\n" ]
                |> List.filter (\x -> x /= "")

        bodyPart =
            String.join "\n" bodyParts
    in
    String.join "\n" [ titlePart, bodyPart ]


renderTableOfContents : LatexState -> List LatexExpression -> String
renderTableOfContents latexState list =
    let
        innerPart =
            makeTableOfContents latexState
    in
    "\n<p class=\"tocTitle\">Table of Contents</p>\n<ul class=\"ListEnvironment\">\n" ++ innerPart ++ "\n</ul>\n"


makeTableOfContents : LatexState -> String
makeTableOfContents latexState =
    List.foldl (\tocItem acc -> acc ++ [ makeTocItem tocItem ]) [] (List.indexedMap (,) latexState.tableOfContents)
        |> String.join "\n"


makeTocItem : ( Int, TocEntry ) -> String
makeTocItem tocItem =
    let
        i =
            Tuple.first tocItem

        ti =
            Tuple.second tocItem

        classProperty =
            "class=\"sectionLevel" ++ toString ti.level ++ "\""

        id =
            makeId (sectionPrefix ti.level) ti.name

        href =
            "href=\"#" ++ id ++ "\""

        innerTag =
            ti.label ++ " " ++ tag "a" href ti.name
    in
    tag "li" classProperty innerTag


sectionPrefix : Int -> String
sectionPrefix level =
    case level of
        1 ->
            "section"

        2 ->
            "subsection"

        3 ->
            "subsubsection"

        _ ->
            "asection"



{- IMAGE HELPERS -}


imageCenterStyle imageAttributes =
    "class=\"center\" style=\"width: " ++ toString (imageAttributes.width + 20) ++ "px; margin-left:auto, margin-right:auto; text-align: center;\""


imageFloatRightStyle imageAttributes =
    "style=\"float: right; width: " ++ toString (imageAttributes.width + 20) ++ "px; margin: 0 0 7.5px 10px; text-align: center;\""


imageFloatLeftStyle imageAttributes =
    "style=\"float: left; width: " ++ toString (imageAttributes.width + 20) ++ "px; margin: 0 10px 7.5px 0; text-align: center;\""


div : List String -> List String -> String
div attributes children =
    let
        attributeString =
            attributes |> String.join " "

        childrenString =
            children |> String.join "\n"
    in
    "<div " ++ attributeString ++ " >\n" ++ childrenString ++ "\n</div>"


img url imageAttributs =
    "<img src=\"" ++ url ++ "\" width=" ++ toString imageAttributs.width ++ " >"


handleCenterImage url label imageAttributes =
    let
        width =
            imageAttributes.width
    in
    div [ imageCenterStyle imageAttributes ] [ img url imageAttributes, "<br>", label ]


handleFloatedImageRight url label imageAttributes =
    let
        width =
            imageAttributes.width

        imageRightDivLeftPart width =
            "<div style=\"float: right; width: " ++ toString (width + 20) ++ "px; margin: 0 0 7.5px 10px; text-align: center;\">"
    in
    imageRightDivLeftPart width ++ "<img src=\"" ++ url ++ "\" width=" ++ toString width ++ "><br>" ++ label ++ "</div>"


handleFloatedImageLeft url label imageAttributes =
    let
        width =
            imageAttributes.width

        imageLeftDivLeftPart width =
            "<div style=\"float: left; width: " ++ toString (width + 20) ++ "px; margin: 0 10px 7.5px 0; text-align: center;\">"
    in
    imageLeftDivLeftPart width ++ "<img src=\"" ++ url ++ "\" width=" ++ toString width ++ "><br>" ++ label ++ "</div>"


type alias ImageAttributes =
    { width : Int, float : String, align : String }


parseImageAttributes : String -> ImageAttributes
parseImageAttributes attributeString =
    let
        kvList =
            KeyValueUtilities.getKeyValueList attributeString

        widthValue =
            KeyValueUtilities.getValue "width" kvList |> String.toInt |> Result.withDefault 200

        floatValue =
            KeyValueUtilities.getValue "float" kvList

        alignValue =
            KeyValueUtilities.getValue "align" kvList
    in
    ImageAttributes widthValue floatValue alignValue


imageAttributes : ImageAttributes -> String -> String
imageAttributes imageAttrs attributeString =
    let
        widthValue =
            imageAttrs.width |> toString

        widthElement =
            if widthValue /= "" then
                "width=" ++ widthValue
            else
                ""
    in
    widthElement
