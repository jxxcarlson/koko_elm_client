module MiniLatex.ParserExperiments exposing (..)

import Parser exposing (..)


parseA : Parser ()
parseA =
    succeed identity
        |= symbol "a"


parseB : Parser ()
parseB =
    succeed identity
        |= symbol "b"


parseAorB : Parser ()
parseAorB =
    oneOf [ parseA, parseB ]


parseAthenB : Parser ()
parseAthenB =
    andThen (\_ -> parseB) parseA


parseMaybeAThenB : Parser ()
parseMaybeAThenB =
    andThen (\_ -> parseB) parseAorB


parseAA : Parser String
parseAA =
    succeed identity
        |= symbol "a"
        |> map (\_ -> "A")


parseBB : Parser String
parseBB =
    succeed identity
        |= symbol "b"
        |> map (\_ -> "B")


parseAAorBB : Parser String
parseAAorBB =
    oneOf [ parseAA, parseBB ]


parseBBB : String -> Parser String
parseBBB s =
    succeed identity
        |= symbol "b"
        |> map (\_ -> s ++ "B")


parseAAthenBB : Parser String
parseAAthenBB =
    andThen (\s -> parseBBB s) parseAA


get : Char -> Parser String
get char =
    keep (Exactly 1) (\c -> c == char)


getAndJoin : Char -> String -> Parser String
getAndJoin char s =
    keep (Exactly 1) (\c -> c == char)
        |> map (\x -> s ++ x)


getAB : Parser String
getAB =
    andThen (\s -> getAndJoin 'b' s) (get 'a')
