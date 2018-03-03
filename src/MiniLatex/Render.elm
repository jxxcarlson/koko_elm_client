module MiniLatex.Render
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
import MiniLatex.ErrorMessages as ErrorMessages
import MiniLatex.Html as Html
import MiniLatex.Image as Image exposing (..)
import MiniLatex.JoinStrings as JoinStrings
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
import MiniLatex.Utility as Utility
import Parser
import Regex
import String.Extra


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
                    render latexState latexExpression |> postProcess

                Err error ->
                    "Error: " ++ toString error
    in
    renderOutput

postProcess : String -> String
postProcess str =
  str 
    |> String.Extra.replace "---" "&mdash;"
    |> String.Extra.replace "--" "&ndash;"
    |> String.Extra.replace "\\&" "&#38"

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

        Macro name optArgs args ->
            renderMacro latexState name optArgs args

        SMacro name optArgs args le ->
            renderSMacro latexState name optArgs args le

        Item level latexExpression ->
            renderItem latexState level latexExpression

        InlineMath str ->
            "$" ++ str ++ "$"

        DisplayMath str ->
            "$$" ++ str ++ "$$"

        Environment name args body ->
            renderEnvironment latexState name args body

        LatexList args ->
            renderLatexList latexState args

        LXString str ->
            str

        LXError error ->
            ErrorMessages.renderError error


renderLatexList : LatexState -> List LatexExpression -> String
renderLatexList latexState args =
    args |> List.map (render latexState) |> JoinStrings.joinList |> postProcess


renderArgList : LatexState -> List LatexExpression -> String
renderArgList latexState args =
    args |> List.map (render latexState) |> List.map (\x -> "{" ++ x ++ "}") |> String.join ""


renderOptArgList : LatexState -> List LatexExpression -> String
renderOptArgList latexState args =
    args |> List.map (render latexState) |> List.map (\x -> "[" ++ x ++ "]") |> String.join ""


itemClass : Int -> String
itemClass level =
    "item" ++ toString level


renderItem : LatexState -> Int -> LatexExpression -> String
renderItem latexState level latexExpression =
    "<li class=\"" ++ itemClass level ++ "\">" ++ render latexState latexExpression ++ "</li>\n"


renderComment : String -> String
renderComment str =
    ""



{- ENVIROMENTS -}


renderEnvironmentDict : Dict.Dict String (LatexState -> List LatexExpression -> LatexExpression -> String)
renderEnvironmentDict =
    Dict.fromList
        [ ( "align", \x a y -> renderAlignEnvironment x y )
        , ( "center", \x a y -> renderCenterEnvironment x y )
        , ( "comment", \x a y -> renderCommentEnvironment x y )
        , ( "indent", \x a y -> renderIndentEnvironment x y )
        , ( "enumerate", \x a y -> renderEnumerate x y )
        , ( "eqnarray", \x a y -> renderEqnArray x y )
        , ( "equation", \x a y -> renderEquationEnvironment x y )
        , ( "itemize", \x a y -> renderItemize x y )
        , ( "listing", \x a y -> renderListing x y )
        , ( "macros", \x a y -> renderMacros x y )
        , ( "quotation", \x a y -> renderQuotation x y )
        , ( "tabular", \x a y -> renderTabular x y )
        , ( "thebibliography", \x a y -> renderTheBibliography x y )
        , ( "maskforweb", \x a y -> renderCommentEnvironment x y )
        , ( "useforweb", \x a y -> renderUseForWeb x y )
        , ( "verbatim", \x a y -> renderVerbatim x y )
        , ( "verse", \x a y -> renderVerse x y )
        ]


environmentRenderer : String -> (LatexState -> List LatexExpression -> LatexExpression -> String)
environmentRenderer name =
    case Dict.get name renderEnvironmentDict of
        Just f ->
            f

        Nothing ->
            renderDefaultEnvironment name


renderEnvironment : LatexState -> String -> List LatexExpression -> LatexExpression -> String
renderEnvironment latexState name args body =
    environmentRenderer name latexState args body


renderDefaultEnvironment : String -> LatexState -> List LatexExpression -> LatexExpression -> String
renderDefaultEnvironment name latexState args body =
    if List.member name [ "theorem", "proposition", "corollary", "lemma", "definition" ] then
        renderTheoremLikeEnvironment latexState name args body
    else
        renderDefaultEnvironment2 latexState name args body


renderIndentEnvironment : LatexState -> LatexExpression -> String
renderIndentEnvironment latexState body =
    Html.div [ "style=\"margin-left:2em\"" ] [ render latexState body ]


renderTheBibliography : LatexState -> LatexExpression -> String
renderTheBibliography latexState body =
    Html.div [ "style=\"\"" ] [ render latexState body ]


renderTheoremLikeEnvironment : LatexState -> String -> List LatexExpression -> LatexExpression -> String
renderTheoremLikeEnvironment latexState name args body =
    let
        r =
            render latexState body

        eqno =
            getCounter "eqno" latexState

        s1 =
            getCounter "s1" latexState

        tno =
            getCounter "tno" latexState

        tnoString =
            if s1 > 0 then
                " " ++ toString s1 ++ "." ++ toString tno
            else
                " " ++ toString tno
    in
    "\n<div class=\"environment\">\n<strong>" ++ String.Extra.toSentenceCase name ++ tnoString ++ "</strong>\n<div class=\"italic\">\n" ++ r ++ "\n</div>\n</div>\n"


renderDefaultEnvironment2 : LatexState -> String -> List LatexExpression -> LatexExpression -> String
renderDefaultEnvironment2 latexState name args body =
    let
        r =
            render latexState body
    in
    "\n<div class=\"environment\">\n<strong>" ++ String.Extra.toSentenceCase name ++ "</strong>\n<div>\n" ++ r ++ "\n</div>\n</div>\n"


renderCenterEnvironment latexState body =
    let
        r =
            render latexState body
    in
    "\n<div class=\"center\" >\n" ++ r ++ "\n</div>\n"


renderCommentEnvironment latexState body =
    ""


renderEquationEnvironment latexState body =
    let
        eqno =
            getCounter "eqno" latexState

        s1 =
            getCounter "s1" latexState

        addendum =
            if eqno > 0 then
                if s1 > 0 then
                    "\\tag{" ++ toString s1 ++ "." ++ toString eqno ++ "}"
                else
                    "\\tag{" ++ toString eqno ++ "}"
            else
                ""

        r =
            render latexState body
    in
    "\n$$\n\\begin{equation}" ++ addendum ++ r ++ "\\end{equation}\n$$\n"


renderAlignEnvironment latexState body =
    let
        r =
            render latexState body |> String.Extra.replace "\\ \\" "\\\\"

        eqno =
            getCounter "eqno" latexState

        s1 =
            getCounter "s1" latexState

        addendum =
            if eqno > 0 then
                if s1 > 0 then
                    "\\tag{" ++ toString s1 ++ "." ++ toString eqno ++ "}"
                else
                    "\\tag{" ++ toString eqno ++ "}"
            else
                ""
    in
    "\n$$\n\\begin{align}\n" ++ addendum ++ r ++ "\n\\end{align}\n$$\n"


renderEqnArray latexState body =
    "\n$$\n" ++ render latexState body ++ "\n$$\n"


renderEnumerate latexState body =
    "\n<ol>\n" ++ render latexState body ++ "\n</ol>\n"


renderItemize latexState body =
    "\n<ul>\n" ++ render latexState body ++ "\n</ul>\n"


renderMacros latexState body =
    "\n$$\n" ++ render latexState body ++ "\n$$\n"


renderQuotation latexState body =
    Html.div [ "class=\"quotation\"" ] [ render latexState body ]


renderVerse latexState body =
    Html.div [ "class=\"verse\"" ] [ String.trim <| render latexState body ]


renderUseForWeb latexState body =
    "\n$$\n" ++ render latexState body ++ "\n$$\n"


renderTabular latexState body =
    renderTableBody body


renderCell cell =
    case cell of
        LXString s ->
            "<td>" ++ s ++ "</td>"

        InlineMath s ->
            "<td>$" ++ s ++ "$</td>"

        _ ->
            "<td>-</td>"


renderRow row =
    case row of
        LatexList row_ ->
            row_
                |> List.foldl (\cell acc -> acc ++ " " ++ renderCell cell) ""
                |> (\row -> "<tr> " ++ row ++ " </tr>\n")

        _ ->
            "<tr>-</tr>"


renderTableBody body =
    case body of
        LatexList body_ ->
            body_
                |> List.foldl (\row acc -> acc ++ " " ++ renderRow row) ""
                |> (\body -> "<table>\n" ++ body ++ "</table>\n")

        _ ->
            "<table>-</table>"


renderVerbatim latexState body =
    let
        body2 =
            render latexState body |> String.Extra.replace ">" "&gt;" |> String.Extra.replace "<" "&lt;"
    in
    "\n<pre class=\"verbatim\">" ++ body2 ++ "</pre>\n"


renderListing latexState body =
    let
        text =
            render latexState body
    in
    "\n<pre class=\"verbatim\">" ++ Utility.addLineNumbers text ++ "</pre>\n"



{- MACROS: DISPATCHERS AND HELPERS -}


renderMacroDict : Dict.Dict String (LatexState -> List LatexExpression -> List LatexExpression -> String)
renderMacroDict =
    Dict.fromList
        [ ( "bozo", \x y z -> renderBozo x z )
        , ( "bigskip", \x y z -> renderBigSkip x z )
        , ( "cite", \x y z -> renderCite x z )
        , ( "code", \x y z -> renderCode x z )
        , ( "comment", \x y z -> renderInlineComment x z )
        , ( "ellie", \x y z -> renderEllie x z )
        , ( "emph", \x y z -> renderItalic x z )
        , ( "eqref", \x y z -> renderEqRef x z )
        , ( "href", \x y z -> renderHRef x z )
        , ( "iframe", \x y z -> renderIFrame x z )
        , ( "image", \x y z -> renderImage x z )
        , ( "imageref", \x y z -> renderImageRef x z )
        , ( "index", \x y z -> "" )
        , ( "italic", \x y z -> renderItalic x z )
        , ( "label", \x y z -> "" )
        , ( "maketitle", \x y z -> "" )
        , ( "tableofcontents", \x y z -> renderTableOfContents x z )
        , ( "maketitle", \x y z -> renderTitle x z )
        , ( "mdash", \x y z -> "&mdash;" )
        , ( "ndash", \x y z -> "&ndash;" )
        , ( "newcommand", \x y z -> renderNewCommand x z )
        , ( "ref", \x y z -> renderRef x z )
        , ( "section", \x y z -> renderSection x z )
        , ( "section*", \x y z -> renderSectionStar x z )
        , ( "setcounter", \x y z -> "" )
        , ( "medskip", \x y z -> renderMedSkip x z )
        , ( "smallskip", \x y z -> renderSmallSkip x z )
        , ( "strong", \x y z -> renderStrong x z )
        , ( "subheading", \x y z -> renderSubheading x z )
        , ( "subsection", \x y z -> renderSubsection x z )
        , ( "subsection*", \x y z -> renderSubsectionStar x z )
        , ( "subsubsection", \x y z -> renderSubSubsection x z )
        , ( "subsubsection*", \x y z -> renderSubSubsectionStar x z )
        , ( "title", \x y z -> "" )
        , ( "author", \x y z -> "" )
        , ( "date", \x y z -> "" )
        , ( "revision", \x y z -> "" )
        , ( "email", \x y z -> "" )
        , ( "term", \x y z -> renderTerm x z )
        , ( "xlink", \x y z -> renderXLink x z )
        , ( "xlinkPublic", \x y z -> renderXLinkPublic x z )
        ]


renderSMacroDict : Dict.Dict String (LatexState -> List LatexExpression -> List LatexExpression -> LatexExpression -> String)
renderSMacroDict =
    Dict.fromList
        [ ( "bibitem", \latexState optArgs args body -> renderBibItem latexState optArgs args body )
        ]



-- SMacro String (List LatexExpression) (List LatexExpression) LatexExpression


macroRenderer : String -> (LatexState -> List LatexExpression -> List LatexExpression -> String)
macroRenderer name =
    case Dict.get name renderMacroDict of
        Just f ->
            f

        Nothing ->
            reproduceMacro name


reproduceMacro : String -> LatexState -> List LatexExpression -> List LatexExpression -> String
reproduceMacro name latexState optArgs args =
    "<span style=\"color: red;\">\\" ++ name ++ renderOptArgList emptyLatexState optArgs ++ renderArgList emptyLatexState args ++ "</span>"


renderMacro : LatexState -> String -> List LatexExpression -> List LatexExpression -> String
renderMacro latexState name optArgs args =
    macroRenderer name latexState optArgs args


renderArg : Int -> LatexState -> List LatexExpression -> String
renderArg k latexState args =
    render latexState (getElement k args) |> String.trim


renderSMacro : LatexState -> String -> List LatexExpression -> List LatexExpression -> LatexExpression -> String
renderSMacro latexState name optArgs args le =
    case Dict.get name renderSMacroDict of
        Just f ->
            f latexState optArgs args le

        Nothing ->
            "<span style=\"color: red;\">\\" ++ name ++ renderArgList emptyLatexState args ++ " " ++ render latexState le ++ "</span>"



{- INDIVIDUAL MACRO RENDERERS -}


renderBozo : LatexState -> List LatexExpression -> String
renderBozo latexState args =
    "bozo{" ++ renderArg 0 latexState args ++ "}{" ++ renderArg 1 latexState args ++ "}"


renderBibItem : LatexState -> List LatexExpression -> List LatexExpression -> LatexExpression -> String
renderBibItem latexState optArgs args body =
    let
        label =
            if List.length optArgs == 1 then
                renderArg 0 latexState optArgs
            else
                renderArg 0 latexState args
    in
    " <p id=\"bib:" ++ label ++ "\">[" ++ label ++ "] " ++ render latexState body ++ "</p>\n"


renderBigSkip : LatexState -> List LatexExpression -> String
renderBigSkip latexState args =
    Html.div [] [ "<br><br>" ]


renderMedSkip : LatexState -> List LatexExpression -> String
renderMedSkip latexState args =
    Html.div [] [ "<br>" ]


renderSmallSkip : LatexState -> List LatexExpression -> String
renderSmallSkip latexState args =
    "<p class=\"smallskip\"> &nbsp;</p>"


{-| Needs work
-}
renderCite : LatexState -> List LatexExpression -> String
renderCite latexState args =
    let
        label_ =
            renderArg 0 latexState args

        ref =
            getDictionaryItem ("bibitem:" ++ label_) latexState

        label =
            if ref /= "" then
                ref
            else
                label_
    in
    " <span>[<a href=\"#bib:" ++ label ++ "\">" ++ label ++ "</a>]</span>"


renderCode : LatexState -> List LatexExpression -> String
renderCode latexState args =
    let
        arg =
            renderArg 0 latexState args
    in
    " <span class=\"code\">" ++ arg ++ "</span>"


renderInlineComment : LatexState -> List LatexExpression -> String
renderInlineComment latexState args =
    ""


renderEllie : LatexState -> List LatexExpression -> String
renderEllie latexState args =
    let
        src =
            "src =\"https://ellie-app.com/embed/" ++ renderArg 0 latexState args ++ "\""

        url =
            "https://ellie-app.com/" ++ renderArg 0 latexState args

        title_ =
            renderArg 1 latexState args

        foo =
            27.99

        title =
            if title_ == "xxx" then
                "Link to Ellie"
            else
                title_

        style =
            " style = \"width:100%; height:400px; border:0; border-radius: 3px; overflow:hidden;\""

        sandbox =
            " sandbox=\"allow-modals allow-forms allow-popups allow-scripts allow-same-origin\""
    in
    "<iframe " ++ src ++ style ++ sandbox ++ " ></iframe>\n<center style=\"margin-top: -10px;\"><a href=\"" ++ url ++ "\" target=_blank>" ++ title ++ "</a></center>"


renderEqRef : LatexState -> List LatexExpression -> String
renderEqRef latexState args =
    let
        key =
            renderArg 0 emptyLatexState args

        ref =
            getCrossReference key latexState
    in
    "$(" ++ ref ++ ")$"


renderHRef : LatexState -> List LatexExpression -> String
renderHRef latexState args =
    let
        url =
            renderArg 0 emptyLatexState args

        label =
            renderArg 1 emptyLatexState args
    in
    "<a href=\"" ++ url ++ "\" target=_blank>" ++ label ++ "</a>"


renderIFrame : LatexState -> List LatexExpression -> String
renderIFrame latexState args =
    let
        url =
            renderArg 0 emptyLatexState args

        src =
            "src =\"" ++ url ++ "\""

        title_ =
            renderArg 1 emptyLatexState args

        title =
            if title_ == "xxx" then
                "Link"
            else
                title_

        height_ =
            renderArg 2 emptyLatexState args

        height =
            if title_ == "xxx" || height_ == "xxx" then
                "400px"
            else
                height_

        sandbox =
            ""

        style =
            " style = \"width:100%; height:" ++ height ++ "; border:1; border-radius: 3px; overflow:scroll;\""
    in
    "<iframe scrolling=\"yes\" " ++ src ++ sandbox ++ style ++ " ></iframe>\n<center style=\"margin-top: 0px;\"><a href=\"" ++ url ++ "\" target=_blank>" ++ title ++ "</a></center>"


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
    String.join ":" [ prefix, compress ":" name ]


{-| map str to lower case and squeeze out bad characters
-}
compress : String -> String -> String
compress replaceBlank str =
    str
        |> String.toLower
        |> String.Extra.replace " " replaceBlank
        |> Regex.replace Regex.All (Regex.regex "[,;.!?&_]") (\_ -> "")


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
        Html.div [ imageFloatLeftStyle imageAttrs ] [ Html.img url imageAttrs, "<br>", label ]
    else if imageAttrs.float == "right" then
        Html.div [ imageFloatRightStyle imageAttrs ] [ Html.img url imageAttrs, "<br>", label ]
    else if imageAttrs.align == "center" then
        Html.div [ imageCenterStyle imageAttrs ] [ Html.img url imageAttrs, "<br>", label ]
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
        Html.a url (Html.div [ imageFloatLeftStyle imageAttrs ] [ Html.img imageUrl imageAttrs ])
    else if imageAttrs.float == "right" then
        Html.a url (Html.div [ imageFloatRightStyle imageAttrs ] [ Html.img imageUrl imageAttrs ])
    else if imageAttrs.align == "center" then
        Html.a url (Html.div [ imageCenterStyle imageAttrs ] [ Html.img imageUrl imageAttrs ])
    else
        Html.a url (Html.div [ imageCenterStyle imageAttrs ] [ Html.img imageUrl imageAttrs ])
