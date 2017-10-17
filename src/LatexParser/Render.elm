module LatexParser.Render exposing (..)

import Dict
import Configuration
import LatexParser.Latex as Latex
import LatexParser.Parser exposing (latex, latexParser, defaultLatexList)
import LatexParser.ParserTypes exposing (Latex(..))
import LatexParser.Image exposing (getKeyValueList, getValue)
import List.Extra
import Regex
import String.Extra
import Parser


type alias CrossReferences =
    Dict.Dict String String


emptyDict =
    Dict.empty


type alias LatexState =
    { s1 : Int, s2 : Int, s3 : Int, tno : Int, eqno : Int, dict : CrossReferences }


emptyLatexState =
    { s1 = 0, s2 = 0, s3 = 0, tno = 0, eqno = 0, dict = Dict.empty }


parseParagraph : String -> List Latex
parseParagraph text =
    Parser.run latexParser text
        |> Result.withDefault defaultLatexList


render : LatexState -> List Latex -> String
render latexState latexParser =
    latexParser
        |> List.map (transformLatex latexState)
        |> String.join ("")
        |> (\x -> "\n<p>\n" ++ x ++ "\n</p>\n")


{-| NB:

transformText str = str |> parseParagraph |> render

-}
transformText : String -> String
transformText text =
    Parser.run latexParser text
        |> Result.withDefault defaultLatexList
        |> List.map (transformLatex emptyLatexState)
        |> String.join ("")
        |> (\x -> "\n<p>\n" ++ x ++ "\n</p>\n")


getAt : Int -> List String -> String
getAt k list_ =
    List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


transformLatex : LatexState -> Latex -> String
transformLatex latexState latex =
    case latex of
        Comment () ->
            ""

        Words str ->
            if List.member str [ ".", ",", ":", ";", "?", "!" ] then
                str ++ " "
            else
                " " ++ str ++ " "

        Macro v ->
            handleMacro latexState v

        Environment v ->
            handleEnvironment latexState v

        InlineMath v ->
            " $" ++ v.value ++ "$"

        DisplayMath v ->
            "\n$$\n" ++ v.value ++ "\n$$\n"


handleEnvironment : LatexState -> { a | body : String, env : String } -> String
handleEnvironment latexState v =
    let
        env =
            v.env

        body =
            v.body
    in
        case env of
            "center" ->
                handleCenterEnvironment body

            "equation" ->
                handleEquationEnvironment latexState body

            "align" ->
                handleAlignEnvironment latexState body

            "itemize" ->
                handleItemize body

            "enumerate" ->
                handleEnumerate body

            "macros" ->
                handleMacros body

            "tabular" ->
                handleTabular body

            "verbatim" ->
                handleVerbatim body

            _ ->
                handleDefaultEnvironment latexState env body


handleCenterEnvironment : String -> String
handleCenterEnvironment body =
    "\n<div class=\"center\">\n" ++ body ++ "\n</div>\n"


handleEquationEnvironment : LatexState -> String -> String
handleEquationEnvironment latexState body =
    let
        addendum =
            if latexState.eqno > 0 then
                if latexState.s1 > 0 then
                    "\\tag{" ++ (toString latexState.s1) ++ "." ++ (toString latexState.eqno) ++ "}\n"
                else
                    "\\tag{" ++ (toString latexState.eqno) ++ "}\n"
            else
                ""
    in
        Debug.log "EQUATION" "\n\\begin{equation}\n" ++ addendum ++ body ++ "\n\\end{equation}\n"


handleAlignEnvironment : LatexState -> String -> String
handleAlignEnvironment latexState body =
    let
        editedBody =
            String.Extra.replace "\\ \\" "\\\\" body

        -- NOTE: ^^^ temporary fix
        addendum =
            if latexState.eqno > 0 then
                if latexState.s1 > 0 then
                    "\\tag{" ++ (toString latexState.s1) ++ "." ++ (toString latexState.eqno) ++ "}"
                else
                    "\\tag{" ++ (toString latexState.eqno) ++ "}"
            else
                ""
    in
        "\n$$\n\\begin{align}\n" ++ addendum ++ editedBody ++ "\n\\end{align}\n$$\n"


parseItems : String -> List String
parseItems str =
    str
        |> String.split "\n"
        |> List.filter (\x -> (String.left 1 x) /= "%")
        |> String.join "\n"
        |> String.split "\\item"
        |> List.map String.trim
        |> List.filter (\x -> x /= "")


tagItem : String -> String -> String
tagItem tag str =
    "\n<" ++ tag ++ ">\n" ++ str ++ "</" ++ tag ++ ">\n"



-- tagItems : String -> List String -> String
-- tagItems tag stringList =
--   stringList |> foldl "" (\x )


handleItemize : String -> String
handleItemize body =
    body
        |> parseItems
        |> List.map transformText
        |> List.map (tagItem "li")
        |> List.foldl (\x acc -> acc ++ x) ""
        |> tagItem "ul"


handleEnumerate : String -> String
handleEnumerate body =
    body
        |> parseItems
        |> List.map transformText
        |> List.map (tagItem "li")
        |> List.reverse
        |> List.foldl (\x acc -> x ++ acc) ""
        |> tagItem "ol"


handleDefaultEnvironment : LatexState -> String -> String -> String
handleDefaultEnvironment latexState env body =
    if List.member env [ "theorem", "proposition", "corollary", "lemma", "definition" ] then
        handleTheoremLikeEnvironment latexState env body
    else
        handleDefaultEnvironment2 env body


handleTheoremLikeEnvironment : LatexState -> String -> String -> String
handleTheoremLikeEnvironment latexState env body =
    let
        body2 =
            body |> String.trim |> transformText

        tnoString =
            if latexState.s1 > 0 then
                " " ++ (toString latexState.s1) ++ "." ++ (toString latexState.tno)
            else
                " " ++ (toString latexState.tno)
    in
        "\n<div class=\"environment\">\n<strong>" ++ (String.Extra.toSentenceCase env) ++ tnoString ++ "</strong>\n<div class=\"italic\">\n" ++ body2 ++ "\n</div>\n</div>\n"


handleDefaultEnvironment2 : String -> String -> String
handleDefaultEnvironment2 env body =
    let
        body2 =
            body |> String.trim |> transformText
    in
        "\n<div class=\"environment\">\n<strong>" ++ (String.Extra.toSentenceCase env) ++ "</strong>\n<div class=\"italic\">\n" ++ body2 ++ "\n</div>\n</div>\n"


handleMacros : String -> String
handleMacros body =
    "\n$$\n" ++ body ++ "\n$$\n"


{-| "cell1 & cell2 & cell3 \ \ncell1 & cell2 & cell3 \ "
=> [["cell1","cell2","cell3"],["cell1","cell2","cell3"]]
-}
parseTabular : String -> List (List String)
parseTabular str =
    let
        argResult =
            Debug.log "args" (Latex.args str)

        argString =
            case argResult of
                Ok args ->
                    args
                        |> String.join ("}{")
                        |> \x -> "{" ++ x ++ "}"

                _ ->
                    ""
    in
        str
            |> String.Extra.replace argString ""
            |> String.split "\n"
            |> List.filter (\x -> x /= "")
            |> List.map (String.Extra.replace "\\" "")
            |> List.map (String.split "&")
            |> List.map (List.map String.trim)


renderTableCell : String -> String
renderTableCell str =
    "<td>" ++ str ++ "</td> "


renderTableRow : List String -> String
renderTableRow data =
    data
        |> List.foldr (\x acc -> renderTableCell x ++ acc) ""
        |> (\x -> "<tr> " ++ x ++ "</tr>\n")


renderTabular : List (List String) -> String
renderTabular data =
    data
        |> List.foldr (\x acc -> (renderTableRow x) ++ acc) ""
        |> (\x -> "<table>\n" ++ x ++ "</table>\n")
        |> (\x -> "<center>\n" ++ x ++ "</center>\n\n")


handleTabular : String -> String
handleTabular body =
    body
        |> parseTabular
        |> renderTabular


handleVerbatim : String -> String
handleVerbatim body =
    "\n<pre class=\"verbatim\">" ++ body ++ "</pre>\n"



-- MACROS


handleMacro : LatexState -> { a | args : List String, name : String } -> String
handleMacro latexState v =
    case v.name of
        "code" ->
            handleCode v.args

        "eqref" ->
            handleEqRef latexState v.args

        "emph" ->
            handleEmph v.args

        "href" ->
            handleHyperlink v.args

        "bibhref" ->
            handleBibHyperlink v.args

        "ellie" ->
            handleEllie v.args

        "image" ->
            handleImage v.args

        "index" ->
            handleIndex v.args

        "italic" ->
            handleItalic v.args

        "mdash" ->
            "&mdash;"

        "newcommand" ->
            handleNewCommand v.args

        "ndash" ->
            "&ndash;"

        "ref" ->
            handleRef latexState v.args

        "section" ->
            handleSection latexState v.args

        "section*" ->
            handleSectionStar v.args

        "setcounter" ->
            ""

        "strong" ->
            handleStrong v.args

        "subheading" ->
            handleSubheading v.args

        "subsection" ->
            handleSubSection latexState v.args

        "subsection*" ->
            handleSubSectionStar v.args

        "subsubsection" ->
            handleSubSubSection latexState v.args

        "subsubsection*" ->
            handleSubSubSectionStar v.args

        "term" ->
            handleTerm v.args

        "title" ->
            handleTitle v.args

        "xlink" ->
            handleXLink v.args

        "xlinkPublic" ->
            handleXLinkPublic v.args

        _ ->
            handleDefault v


handleBibHyperlink args =
    let
        url =
            getAt 0 args

        label =
            getAt 1 args

        ref =
            getAt 2 args
    in
        "\n<p class= \"bibhref\">[" ++ ref ++ "] <a href=\"" ++ url ++ " target=_blank\">" ++ label ++ "</a>\n</p>\n"


handleDefault v =
    case (List.length v.args) of
        0 ->
            handleBareCommand v.name

        1 ->
            handleOneArgCommand v.name v.args

        _ ->
            handleTwoArgCommand v.name v.args


handleBareCommand : String -> String
handleBareCommand name =
    "\\" ++ name


handleOneArgCommand : String -> List String -> String
handleOneArgCommand name args =
    let
        arg =
            getAt 0 args
    in
        "\\" ++ name ++ "{" ++ arg ++ "}"


handleTwoArgCommand : String -> List String -> String
handleTwoArgCommand name args =
    let
        arg1 =
            getAt 0 args

        arg2 =
            getAt 0 args
    in
        "\\" ++ name ++ "{" ++ arg1 ++ "}" ++ "{" ++ arg2 ++ "}"


handleBold : List String -> String
handleBold args =
    let
        arg =
            getAt 0 args
    in
        "<strong>" ++ arg ++ "</strong>"


handleCode : List String -> String
handleCode args =
    let
        arg =
            getAt 0 args
    in
        "<code>" ++ arg ++ "</code>"


handleEmph : List String -> String
handleEmph args =
    let
        arg =
            getAt 0 args
    in
        " <b>" ++ arg ++ "</b>"


handleHyperlink : List String -> String
handleHyperlink args =
    let
        url =
            getAt 0 args

        label =
            getAt 1 args
    in
        " <a href=\"" ++ url ++ "\" target=_blank>" ++ label ++ "</a>"


handleEllie : List String -> String
handleEllie args =
    let
        src =
            "src =\"https://ellie-app.com/embed/" ++ (getAt 0 args) ++ "\""

        url =
            "https://ellie-app.com/" ++ (getAt 0 args)

        title_ =
            getAt 1 args

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


handleImage : List String -> String
handleImage args =
    let
        url =
            getAt 0 args

        label =
            getAt 1 args

        attributeString =
            getAt 2 args

        imageAttrs =
            Debug.log "imageAttrs" (parseImageAttributes attributeString)
    in
        if imageAttrs.float == "left" then
            handleFloatedImageLeft url label imageAttrs
        else if imageAttrs.float == "right" then
            handleFloatedImageRight url label imageAttrs
        else if imageAttrs.align == "center" then
            handleCenterImage url label imageAttrs
        else
            "<image src=\"" ++ url ++ "\" " ++ (imageAttributes imageAttrs attributeString) ++ " >"


handleCenterImage url label imageAttributes =
    let
        width =
            imageAttributes.width

        imageCenterLeftPart width =
            "<div class=\"center\" style=\"width: " ++ (toString (width + 20)) ++ "px; margin: 0 10px 7.5px 10px; text-align: center;\">"
    in
        (imageCenterLeftPart width) ++ "<img src=\"" ++ url ++ "\" width=" ++ (toString width) ++ "><br>" ++ label ++ "</div>"


handleFloatedImageRight url label imageAttributes =
    let
        width =
            imageAttributes.width

        imageRightDivLeftPart width =
            "<div style=\"float: right; width: " ++ (toString (width + 20)) ++ "px; margin: 0 0 7.5px 10px; text-align: center;\">"
    in
        (imageRightDivLeftPart width) ++ "<img src=\"" ++ url ++ "\" width=" ++ (toString width) ++ "><br>" ++ label ++ "</div>"


handleFloatedImageLeft url label imageAttributes =
    let
        width =
            imageAttributes.width

        imageLeftDivLeftPart width =
            "<div style=\"float: left; width: " ++ (toString (width + 20)) ++ "px; margin: 0 10px 7.5px 0; text-align: center;\">"
    in
        (imageLeftDivLeftPart width) ++ "<img src=\"" ++ url ++ "\" width=" ++ (toString width) ++ "><br>" ++ label ++ "</div>"


type alias ImageAttributes =
    { width : Int, float : String, align : String }


parseImageAttributes : String -> ImageAttributes
parseImageAttributes attributeString =
    let
        kvList =
            LatexParser.Image.getKeyValueList attributeString

        widthValue =
            LatexParser.Image.getValue "width" kvList |> String.toInt |> Result.withDefault 200

        floatValue =
            LatexParser.Image.getValue "float" kvList

        alignValue =
            LatexParser.Image.getValue "align" kvList
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
        -- String.join " " [ styleElement, widthElement ]
        widthElement


handleIndex : List String -> String
handleIndex args =
    ""


handleItalic : List String -> String
handleItalic args =
    let
        arg =
            getAt 0 args
    in
        " <span class=italic>" ++ arg ++ "</span>"


handleNewCommand : List String -> String
handleNewCommand args =
    let
        command =
            getAt 0 args

        definition =
            getAt 1 args
    in
        "\\newcommand{" ++ command ++ "}{" ++ definition ++ "}"


handleRef : LatexState -> List String -> String
handleRef latexState args =
    let
        key =
            getAt 0 args
    in
        Dict.get key latexState.dict |> Maybe.withDefault "??"


handleEqRef : LatexState -> List String -> String
handleEqRef latexState args =
    let
        key =
            getAt 0 args

        value =
            Dict.get key latexState.dict |> Maybe.withDefault "??"
    in
        "$(" ++ value ++ ")$"


handleStrong : List String -> String
handleStrong args =
    let
        arg =
            getAt 0 args
    in
        " <span class=\"strong\">" ++ arg ++ "</span>"


handleSection : LatexState -> List String -> String
handleSection latexState args =
    let
        arg =
            getAt 0 args

        addendum =
            if latexState.s1 > 0 then
                (toString latexState.s1) ++ " "
            else
                ""
    in
        "<h2>" ++ addendum ++ arg ++ "</h2>"


handleSectionStar : List String -> String
handleSectionStar args =
    let
        arg =
            getAt 0 args
    in
        "<h2>" ++ arg ++ "</h2>"


handleSubheading : List String -> String
handleSubheading args =
    let
        arg =
            getAt 0 args
    in
        "<div class=\"subheading\">" ++ arg ++ "</div>"


handleSubSection : LatexState -> List String -> String
handleSubSection latexState args =
    let
        arg =
            getAt 0 args

        addendum =
            if latexState.s1 > 0 then
                (toString latexState.s1) ++ "." ++ (toString latexState.s2) ++ " "
            else
                ""
    in
        "<h3>" ++ addendum ++ arg ++ "</h3>"


handleSubSectionStar : List String -> String
handleSubSectionStar args =
    let
        arg =
            getAt 0 args
    in
        "<h3>" ++ arg ++ "</h3>"


handleSubSubSection : LatexState -> List String -> String
handleSubSubSection latexState args =
    let
        arg =
            getAt 0 args

        addendum =
            if latexState.s1 > 0 then
                (toString latexState.s1) ++ "." ++ (toString latexState.s2) ++ "." ++ (toString latexState.s3) ++ " "
            else
                ""
    in
        "<h4>" ++ addendum ++ arg ++ "</h4>"


handleSubSubSectionStar : List String -> String
handleSubSubSectionStar args =
    let
        arg =
            getAt 0 args
    in
        "<h4>" ++ arg ++ "</h4>"


handleTitle : List String -> String
handleTitle args =
    let
        arg =
            getAt 0 args
    in
        "<h1>" ++ arg ++ "</h1>"


handleTerm : List String -> String
handleTerm args =
    let
        arg =
            getAt 0 args
    in
        " <span class=italic>" ++ arg ++ "</span>"


handleXLink : List String -> String
handleXLink args =
    let
        id =
            getAt 0 args

        label =
            getAt 1 args
    in
        " <a href=\"" ++ Configuration.client ++ "##document/" ++ id ++ "\">" ++ label ++ "</a>"


handleXLinkPublic : List String -> String
handleXLinkPublic args =
    let
        id =
            getAt 0 args

        label =
            getAt 1 args
    in
        " <a href=\"" ++ Configuration.client ++ "##public/" ++ id ++ "\">" ++ label ++ "</a>"
