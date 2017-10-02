module LatexParser.Render exposing (..)

import LatexParser.Parser exposing (Latex(..), latex, latexList, latexListGet)
import LatexParser.Image exposing (getKeyValueList, getValue)
import List.Extra
import String.Extra
import Parser


transformText : String -> String
transformText text =
    Parser.run latexList text
        --|> Debug.log "latexList"
        |> latexListGet
        --|> Debug.log "latexListGet"
        |> List.map transformLatex
        --|> Debug.log "transformLatex"
        |> String.join (" ")



-- |> Debug.log "String.join"


getAt : Int -> List String -> String
getAt k list_ =
    List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


transformLatex : Latex -> String
transformLatex latex =
    case latex of
        Comment () ->
            ""

        Word str ->
            str

        Macro v ->
            handleMacro v

        Environment v ->
            handleEnvironment v

        InlineMath v ->
            " $" ++ v.value ++ "$ "

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



-- "\nA(i \\to f, t) &= (| U_0(t)U_I(t) | i ) \\ \\\n&=(U_0(t)^ \\dagger f | U_I(t) | i) \\ \\\n&= e^{-i \\omega_ft}( f | U_I(t) | i )\n"


handleDefaultEnvironment : String -> String -> String
handleDefaultEnvironment env body =
    "\n<strong>" ++ (String.Extra.toSentenceCase env) ++ "</strong>\n<div class=\"italic\">\n" ++ body ++ "\n</div>\n"


handleMacros : String -> String
handleMacros body =
    "\n$$\n" ++ body ++ "\n$$\n"


handleVerbatim : String -> String
handleVerbatim body =
    "\n<pre>" ++ body ++ "</pre>\n"



-- MACROS


handleMacro : { a | args : List String, name : String } -> String
handleMacro v =
    -- let
    --     _ =
    --         Debug.log "handleMacro" v
    -- in
    case v.name of
        "code" ->
            handleCode v.args

        "emph" ->
            handleEmph v.args

        "hyperlink" ->
            handleHyperlink v.args

        "image" ->
            handleImage v.args

        "italic" ->
            handleItalic v.args

        "newcommand" ->
            handleNewCommand v.args

        "section" ->
            handleSection v.args

        "strong" ->
            handleStrong v.args

        "subsection" ->
            handleSubSection v.args

        "subsubsection" ->
            handleSubSubSection v.args

        "subsubsubsection" ->
            handleSubSubSubSection v.args

        _ ->
            handleDefault v


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
        "<b>" ++ arg ++ "</b>"


handleHyperlink : List String -> String
handleHyperlink args =
    let
        url =
            getAt 0 args

        label =
            getAt 1 args
    in
        "<a href=\"" ++ url ++ " target=_blank\">" ++ label ++ "</a>"


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
            if floatValue /= "" then
                "style=float:" ++ floatValue
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
        "<span class=italic>" ++ arg ++ "</span>"


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
        "<strong>" ++ arg ++ "</strong>"


handleSection : List String -> String
handleSection args =
    let
        arg =
            getAt 0 args
    in
        "<h1>" ++ arg ++ "</h1>"


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
