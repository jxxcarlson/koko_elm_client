module LatexParser.Render exposing (..)

import Configuration
import LatexParser.Parser exposing (Latex(..), latex, latexList, latexListGet)
import LatexParser.Image exposing (getKeyValueList, getValue)
import List.Extra
import Regex
import String.Extra
import Parser


transformText : String -> String
transformText text =
    Parser.run latexList text
        |> latexListGet
        |> List.map transformLatex
        |> String.join ("")
        |> (\x -> "\n<p>\n" ++ x ++ "\n</p>\n")


getAt : Int -> List String -> String
getAt k list_ =
    List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


transformLatex : Latex -> String
transformLatex latex =
    case latex of
        Comment () ->
            ""

        Word str ->
            if Regex.contains (Regex.regex "[.,:;?!]") str then
                str ++ " "
            else
                " " ++ str ++ " "

        Macro v ->
            handleMacro v

        Environment v ->
            handleEnvironment v

        InlineMath v ->
            "$" ++ v.value ++ "$"

        DisplayMath v ->
            "\n$$\n" ++ v.value ++ "\n$$\n"

        _ ->
            "ERR"



-- ENVIRONMENTS


handleEnvironment : { a | body : String, env : String } -> String
handleEnvironment v =
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
                handleEquationEnvironment body

            "align" ->
                handleAlignEnvironment body

            "itemize" ->
                handleItemize body

            "enumerate" ->
                handleEnumerate body

            "macros" ->
                handleMacros body

            "verbatim" ->
                handleVerbatim body

            _ ->
                handleDefaultEnvironment env body


handleCenterEnvironment : String -> String
handleCenterEnvironment body =
    "\n<div class=\"center\">\n" ++ body ++ "\n</div>\n"


handleEquationEnvironment : String -> String
handleEquationEnvironment body =
    "\n\\begin{equation}\n" ++ body ++ "\n\\end{equation}\n"


handleAlignEnvironment : String -> String
handleAlignEnvironment body =
    let
        editedBody =
            String.Extra.replace "\\ \\" "\\\\" body

        -- NOTE: ^^^ temporary fix
    in
        "\n$$\n\\begin{align}\n" ++ editedBody ++ "\n\\end{align}\n$$\n"


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


handleDefaultEnvironment : String -> String -> String
handleDefaultEnvironment env body =
    let
        body2 =
            body |> String.trim |> transformText
    in
        "\n<div class=\"environment\">\n<strong>" ++ (String.Extra.toSentenceCase env) ++ "</strong>\n<div class=\"italic\">\n" ++ body2 ++ "\n</div>\n</div>\n"


handleMacros : String -> String
handleMacros body =
    "\n$$\n" ++ body ++ "\n$$\n"


handleVerbatim : String -> String
handleVerbatim body =
    "\n<pre class=\"verbatim\">" ++ body ++ "</pre>\n"



-- MACROS


handleMacro : { a | args : List String, name : String } -> String
handleMacro v =
    case v.name of
        "code" ->
            handleCode v.args

        "emph" ->
            handleEmph v.args

        "hyperlink" ->
            handleHyperlink v.args

        "bibhyperlink" ->
            handleBibHyperlink v.args

        "ellie" ->
            handleEllie v.args

        "image" ->
            handleImage v.args

        "italic" ->
            handleItalic v.args

        "mdash" ->
            "&mdash;"

        "newcommand" ->
            handleNewCommand v.args

        "ndash" ->
            "&ndash;"

        "section" ->
            handleSection v.args

        "strong" ->
            handleStrong v.args

        "subheading" ->
            handleSubheading v.args

        "subsection" ->
            handleSubSection v.args

        "subsubsection" ->
            handleSubSubSection v.args

        "subsubsubsection" ->
            handleSubSubSubSection v.args

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
        "\n<p class= \"bibhyperlink\">[" ++ ref ++ "] <a href=\"" ++ url ++ " target=_blank\">" ++ label ++ "</a>\n</p>\n"


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

        style =
            " style = \"width:100%; height:400px; border:0; border-radius: 3px; overflow:hidden;\""

        sandbox =
            " sandbox=\"allow-modals allow-forms allow-popups allow-scripts allow-same-origin\""
    in
        "<iframe " ++ src ++ style ++ sandbox ++ " ></iframe>\n<center style=\"margin-top: -10px;\"><a href=\"" ++ url ++ "\" target=_blank>Link to Ellie</a></center>"


handleImage : List String -> String
handleImage args =
    let
        url =
            getAt 0 args

        label =
            getAt 1 args

        attributeString =
            getAt 2 args

        parsedAttributes =
            parseImageAttributes attributeString
    in
        "<image src=\"" ++ url ++ "\" " ++ parsedAttributes ++ " >"


parseImageAttributes : String -> String
parseImageAttributes attributeString =
    let
        kvList =
            LatexParser.Image.getKeyValueList attributeString

        widthValue =
            LatexParser.Image.getValue "width" kvList

        floatValue =
            LatexParser.Image.getValue "float" kvList

        widthElement =
            if widthValue /= "" then
                "width=" ++ widthValue
            else
                ""

        styleElement =
            if floatValue /= "right" then
                "style=float:" ++ floatValue ++ " ;margin-left: 20px"
            else if floatValue /= "left" then
                "style=float:" ++ floatValue ++ " ;margin-right: 20px"
            else
                ""
    in
        String.join " " [ styleElement, widthElement ]



-- parseAttributes str =
--   String.split " "
--
-- captionedImage : String -> String -> String -> String
-- captionedImage url label attributes =
--   let
--
--
--   in


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


handleStrong : List String -> String
handleStrong args =
    let
        arg =
            getAt 0 args
    in
        " <span class=\"strong\">" ++ arg ++ "</span>"


handleSection : List String -> String
handleSection args =
    let
        arg =
            getAt 0 args
    in
        "<h1>" ++ arg ++ "</h1>"


handleSubheading : List String -> String
handleSubheading args =
    let
        arg =
            getAt 0 args
    in
        "<div class=\"subheading\">" ++ arg ++ "</div>"


handleSubSection : List String -> String
handleSubSection args =
    let
        arg =
            getAt 0 args
    in
        "<h2>" ++ arg ++ "</h2>"


handleSubSubSection : List String -> String
handleSubSubSection args =
    let
        arg =
            getAt 0 args
    in
        "<h3>" ++ arg ++ "</h3>"


handleSubSubSubSection : List String -> String
handleSubSubSubSection args =
    let
        arg =
            getAt 0 args
    in
        "<h3>" ++ arg ++ "</h3>"


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
