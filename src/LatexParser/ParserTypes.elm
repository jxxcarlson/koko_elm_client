module LatexParser.ParserTypes exposing (..)

{-| For LateX
-}


type alias Environment_ =
    { env : String
    , body : String
    }


type alias InlineMath_ =
    { value : String
    }


type alias DisplayMath_ =
    { value : String
    }


type alias Macro_ =
    { name : String
    , args : List String
    }


{-| For Parser
-}
type Latex
    = Macro Macro_
    | Environment Environment_
    | InlineMath InlineMath_
    | DisplayMath DisplayMath_
    | Words String
    | Comment ()


type ParserItem
    = String
    | List Latex
