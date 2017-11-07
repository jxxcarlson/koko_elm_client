module Experiment.NestedList exposing (..)


exprInt : Parser Expr
exprInt =
    succeed identity
        |. spaces
        |= int
        |. spaces
        |> map ExprInt


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


exprList : Parser Expr
exprList =
    (delayedCommit (symbol "[") <|
        succeed identity
            |. spaces
            |= repeat oneOrMore (lazy (\_ -> oneOf [ exprInt, exprList ]))
            |. symbol "]"
            |. spaces
    )
        |> map ExprIntList


print parser e =
    case run parser e of
        Ok expr ->
            printHelper expr

        Err err ->
            "Error: " ++ (toString err)


printHelper : Expr -> String
printHelper e =
    case e of
        ExprInt n ->
            toString n

        ExprIntList list ->
            "(" ++ (List.map printHelper list |> String.join ",") ++ ")"
