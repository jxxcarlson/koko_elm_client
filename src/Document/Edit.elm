module Document.Edit
    exposing
        (-- makeReplacements
         -- , migrateTextFomAsciidocLaTeX
         ..
        )

import Types exposing (Document)
import List.Extra
import Regex
import String.Extra


getAt : Int -> List String -> String
getAt k list_ =
    List.Extra.getAt k list_ |> Maybe.withDefault "xxx"


equationRegexString =
    -- "\n\\[env\\.equation\\]\n--\n(.+?)\n--\n"
    "\n\\[env\\.equation\\]\n--\n(.+?)\n--\n"


alignRegexString =
    "\n\\[env\\.equationalign\\]\n--\n(.+?)\n--\n"


labeledEquationRegexString =
    "\n\\[env\\.equation\\#(.+?)\\]\n--\n(.+?)\n--\n"


labeledAlignRegexString =
    "\n\\[env\\.equationalign\\#(.+?)\\]\n--\n(.+?)\n--\n"


hyperlinkRegexString =
    "(http.+?)\\[(.*?)\\] "



-- "(http:.+?) "


sectionRegexString =
    "\n== (.+?)\n\n"


subSectionRegexString =
    "\n=== (.+?)\n\n"


subSubSectionRegexString =
    "\n==== (.+?)\n\n"


testString =
    "foo\n[env.equation]\n--\nla di dah\n--\nbar\n[env.equation]\n--\nho ho ho\n--\nha ha ha\n"


testString2 =
    "foo http://yada[bada] la di dah http://foo[bar] ho ho ho"


testString3 =
    """
Consider next an operator $\\Omega$.  Write it as
[env.equation]
--
\\Omega = \\left(\\sum_m  | m \\ket \\bra m |\\right) \\Omega \\left(\\sum_n  | n \\ket \\bra n | \\right) \\
= \\sum_{m,n}  |m | \\ket \\bra m |  \\Omega | n \\ket \\bra n | = \\sum_m  | m\\ket   \\Omega_{m.n} \\bra n |
--
The operator  is determined by its
"""


matches =
    [ { match = "[env.equation]\n--\nla di dah\n--\n", submatches = [ Just "la di dah" ], index = 4, number = 1 }, { match = "[env.equation]\n--\nho ho ho\n--\n", submatches = [ Just "ho ho ho" ], index = 39, number = 2 } ]



-- replaceExpression : String -> String -> String


getMatchData regexString text =
    text
        |> Regex.find Regex.All (Regex.regex regexString)


getMatches matchData =
    matchData
        |> List.map (\x -> x.match)


getSubMatches matchData =
    matchData
        |> List.map (\x -> x.submatches)
        |> List.filterMap List.head
        |> List.filterMap (\x -> x)


getSubMatches2 matchData =
    matchData
        |> List.map (\x -> x.submatches)
        |> List.map (List.filterMap (\x -> x))


getMatchTuples regexString text =
    let
        matchData =
            getMatchData regexString text

        matches =
            getMatches matchData

        submatches =
            getSubMatches matchData

        zip =
            List.map2 (,)
    in
        zip matches submatches


getMatchTuples2 regexString text =
    let
        matchData =
            getMatchData regexString text

        matches =
            getMatches matchData

        submatches =
            getSubMatches2 matchData

        zip =
            List.map2 (,)
    in
        zip matches submatches


replaceWithTuple tuple pre post text =
    let
        ( target, core ) =
            tuple

        replacement =
            pre ++ core ++ post
    in
        String.Extra.replace target replacement text


type alias Tuple2 =
    ( String, List String )


replaceWithTuple2 : Tuple2 -> String -> String -> String -> String -> String -> String
replaceWithTuple2 tuple2 pre1 post1 pre2 post2 text =
    let
        ( target, argList ) =
            tuple2

        args =
            case List.length argList of
                0 ->
                    Nothing

                1 ->
                    Nothing

                2 ->
                    Just (List.take 2 argList)

                _ ->
                    Nothing

        replacement =
            case args of
                Nothing ->
                    target

                Just aargs ->
                    pre1 ++ (getAt 0 aargs) ++ post1 ++ pre2 ++ (getAt 1 aargs) ++ post2
    in
        String.Extra.replace target replacement text



{-

   makeReplacements equationRegexString "\n\\begin{equation}\n" "\n\\end{equation}\n" testString
-}


makeReplacements regexString pre post text =
    let
        tuples =
            getMatchTuples regexString text
    in
        tuples |> List.foldr (\tuple text -> replaceWithTuple tuple pre post text) text


makeReplacements2 regexString pre1 post1 pre2 post2 text =
    let
        tuples =
            getMatchTuples2 regexString text
    in
        tuples |> List.foldr (\tuple text -> replaceWithTuple2 tuple pre1 post1 pre2 post2 text) text


fixHyperlinks text =
    makeReplacements2 hyperlinkRegexString "\\hyperlink{" "}" "{" "} " text


fixEquations text =
    makeReplacements equationRegexString "\n\n\\begin{equation}\n" "\n\\end{equation}\n\n" text


fixLabeledEquations text =
    makeReplacements2 labeledEquationRegexString "\n\\begin{equation}\n\\label{" "}\n" "" "\n\\end{equation}\n\n" text


fixAligns text =
    makeReplacements alignRegexString "\n\\begin{align}\n" "\n\\end{align}\n\n" text


fixLabeledAligns text =
    makeReplacements2 labeledAlignRegexString "\n\\begin{align}\n\\label{" "}\n" "" "\n\\end{align}\n\n" text


fixSections text =
    makeReplacements sectionRegexString "\n\\section{" "}\n\n" text


fixSubSections text =
    makeReplacements subSectionRegexString "\n\\subsection{" "}\n\n" text


fixSubSubSections text =
    makeReplacements subSubSectionRegexString "\n\\subsubsection{" "}\n\n" text


migrateTextFomAsciidocLaTeX text =
    text
        |> fixEquations
        |> fixLabeledEquations
        |> fixSections
        |> fixSubSections
        |> fixSubSubSections
        |> fixHyperlinks



-- [{ match = "::foo::", submatches = [Just "foo"], index = 9, number = 1 }]
