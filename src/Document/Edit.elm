module Document.Edit
    exposing
        ( makeReplacements
        , migrateTextFomAsciidocLaTeX
        )

import Types exposing (Document)
import Regex
import String.Extra


equationRegexString =
    "\\[env\\.equation\\]\n--\n(.+?)\n--\n"


sectionRegexString =
    "\n== (.+?)\n\n"


subSectionRegexString =
    "\n=== (.+?)\n\n"


subSubSectionRegexString =
    "\n==== (.+?)\n\n"


testString =
    "foo\n[env.equation]\n--\nla di dah\n--\nbar\n[env.equation]\n--\nho ho ho\n--\nha ha ha\n"


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


replaceWithTuple tuple pre post text =
    let
        ( target, core ) =
            tuple

        replacement =
            pre ++ core ++ post
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


fixEquations text =
    makeReplacements equationRegexString "\n\\begin{equation}\n" "\n\\end{equation}\n\n" text


fixSections text =
    makeReplacements sectionRegexString "\n\\section{" "}\n\n" text


fixSubSections text =
    makeReplacements subSectionRegexString "\n\\subsection{" "}\n\n" text


fixSubSubSections text =
    makeReplacements subSubSectionRegexString "\n\\subsubsection{" "}\n\n" text


migrateTextFomAsciidocLaTeX text =
    text
        |> fixEquations
        |> fixSections
        |> fixSubSections
        |> fixSubSubSections



-- [{ match = "::foo::", submatches = [Just "foo"], index = 9, number = 1 }]
