module LatexParser.Experimental exposing (..)

--
-- import LatexParser.Render as Render
-- import LatexParser.Parser
-- import String.Extra

import Parser exposing (..)


type Boolean
    = MyTrue
    | MyFalse
    | MyOr Boolean Boolean


boolean : Parser Boolean
boolean =
    oneOf
        [ succeed MyTrue
            |. keyword "true"
        , succeed MyFalse
            |. keyword "false"
        , succeed MyOr
            |. symbol "("
            |. spaces
            |= lazy (\_ -> boolean)
            |. spaces
            |. symbol "||"
            |. spaces
            |= lazy (\_ -> boolean)
            |. spaces
            |. symbol ")"
        ]


spaces : Parser ()
spaces =
    ignore zeroOrMore (\char -> char == ' ')



-- f x =
--     2 * x
--
--
-- g x =
--     x
--
--
-- {-| accumulator1:
--
-- > accumulator1 f g [1,2,3,4]
-- > ([2,4,6,8],10) : ( List Int, Int )
--
-- -}
-- accumulator1 : (a -> b) -> (a -> Int) -> List a -> ( List b, Int )
-- accumulator1 f g inputList =
--     inputList |> List.foldl (\x acc -> ( (Tuple.first acc) ++ [ f x ], (Tuple.second acc) + (g x) )) ( [], 0 )
--
--
-- type alias LatexState =
--     { s1 : Int, s2 : Int, s3 : Int }
--
--
--
-- {-
--
--    ALGORITHM.
--
--    if section, increment s1, reset s2, s3
--    if subsection, increment s2, reset s3
--    if subsubsection, increment s3
-- -}
--
--
-- accumulator2 : (a -> LatexState -> b) -> (a -> LatexState -> LatexState) -> List a -> ( List b, LatexState )
-- accumulator2 f g inputList =
--     inputList
--         |> List.foldl (\x acc -> ( (Tuple.first acc) ++ [ f x (Tuple.second acc) ], g x (Tuple.second acc) )) ( [], { s1 = 0, s2 = 0, s3 = 0 } )
--
--
-- ff x state =
--     x + state.s3
--
--
-- gg x state =
--     LatexState (x + state.s1) (x + state.s2) (state.s3 + 1)
--
--
-- transformer f g x acc =
--     let
--         ( a, b ) =
--             acc
--     in
--         ( a ++ [ f x b ], g x b )
--
--
--
-- {-
--
--    > import LatexParser.Experimental exposing(..)
--    > [1, 2, 3] |> accumulator3 ff gg
--    > ([1,3,5],{ s1 = 6, s2 = 6, s3 = 3 })
--
--        : ( List Int, LatexParser.Experimental.Status )
--
-- -}
--
--
-- accumulator3 : (a -> LatexState -> b) -> (a -> LatexState -> LatexState) -> List a -> ( List b, LatexState )
-- accumulator3 processor stateUpdater inputList =
--     inputList
--         |> List.foldl (transformer processor stateUpdater) ( [], { s1 = 0, s2 = 0, s3 = 0 } )
--
--
-- processParagraph1 : String -> LatexState -> List LatexParser.Parser.Latex
-- processParagraph1 paragraph latexState =
--     Render.parseParagraph paragraph
--
--
-- updateLateState1 : String -> LatexState -> LatexState
-- updateLateState1 paragraph latexState =
--     latexState
--
--
-- accumulator4 : (a -> LatexState -> b) -> (a -> LatexState -> LatexState) -> List a -> ( List b, LatexState )
-- accumulator4 processor stateUpdater inputList =
--     inputList
--         |> List.foldl (transformer processor stateUpdater) ( [], { s1 = 0, s2 = 0, s3 = 0 } )
