module LatexParser.Tree exposing (..)

{-| General Trees

> e = empty
> Empty : LatexParser.Parser.Tree a

> r = singleton "red"
> Node "red" [Empty] : LatexParser.Parser.Tree String
> g = singleton "green"
> Node "green" [Empty] : LatexParser.Parser.Tree String
> b = singleton "blue"
> Node "blue" [Empty] : LatexParser.Parser.Tree String

> colors = Node "colors" [r,g,b]
> Node "colors" ([Node "red" [Empty],Node "green" [Empty],Node "blue" [Empty]]

    : LatexParser.Parser.Tree String

OR BETTER:

> colors = Node "colors" (List.map singleton ["red", "green", "blue"])
> Node "colors" ([Node "red" [Empty],Node "green" [Empty],Node "blue" [Empty]])

    : LatexParser.Tree.Tree String

-}


type Tree a
    = Empty
    | Node a (List (Tree a))


empty : Tree a
empty =
    Empty


singleton : a -> Tree a
singleton v =
    Node v [ Empty ]


insert : a -> Tree a -> Tree a
insert item tree =
    case tree of
        Empty ->
            singleton item

        Node x [] ->
            Node x [ singleton item ]

        Node x items ->
            Node x (items ++ [ singleton item ])


insertTree : Tree a -> Tree a -> Tree a
insertTree source target =
    case target of
        Empty ->
            source

        Node x [] ->
            Node x [ source ]

        Node x trees ->
            Node x (trees ++ [ source ])


outline : Tree a -> String
outline tree =
    case tree of
        Empty ->
            ""

        Node x [] ->
            (toString x)

        Node x children ->
            (toString x) ++ (List.map (outline >> (\x -> "=" ++ x)) children |> String.join ("\n"))
