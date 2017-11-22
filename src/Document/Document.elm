module Document.Document
    exposing
        ( defaultDocument
        , defaultMasterDocument
        , pageNotFoundDocument
        , errorDocument
        , startDocument
        , emptyDocument
        , blankDocument
        , hasTag
        )

import Types exposing (Document, DocumentAttributes)
import Regex
import String.Extra


hasTag : String -> Document -> Bool
hasTag tagg document =
    List.any (\x -> x == tagg) document.tags


defaultAttributes : DocumentAttributes
defaultAttributes =
    DocumentAttributes False "adoc" "standard" 0


defaultDocument : Document
defaultDocument =
    { id = 0
    , identifier = "nullDocument"
    , author_id = 0
    , author_name = ""
    , title = "Welcome"
    , content = "Welcome to noteshare"
    , rendered_content = "Welcome to noteshare"
    , attributes = defaultAttributes
    , tags = []
    , children = []
    , parent_id = 0
    , parent_title = "String"
    }


errorDocument =
    { id = 0
    , identifier = "errorDocument"
    , author_id = 0
    , author_name = ""
    , title = "Error"
    , content = "Oops - something went wrong"
    , rendered_content = "Oops - something went wrong"
    , attributes = defaultAttributes
    , tags = []
    , children = []
    , parent_id = 0
    , parent_title = "String"
    }


pageNotFoundDocument : Document
pageNotFoundDocument =
    { id = 0
    , identifier = "pageNotFoundDocument"
    , author_id = 0
    , author_name = ""
    , title = "Page not found"
    , content = "Sorry, page not found"
    , rendered_content = "Sorry, page not found"
    , attributes = defaultAttributes
    , tags = []
    , children = []
    , parent_id = 0
    , parent_title = "String"
    }


startDocument : Document
startDocument =
    { id = 0
    , identifier = "nullDocument"
    , author_id = 0
    , author_name = ""
    , title = "Welcome"
    , content = "Welcome to noteshare!"
    , rendered_content = "Welcome to noteshare!"
    , attributes = defaultAttributes
    , tags = []
    , children = []
    , parent_id = 0
    , parent_title = "String"
    }


blankDocument : Document
blankDocument =
    { id = 0
    , identifier = "blank"
    , author_id = 0
    , author_name = ""
    , title = "New Document"
    , content = "Write content here"
    , rendered_content = "Write content here"
    , attributes = defaultAttributes
    , tags = []
    , children = []
    , parent_id = 0
    , parent_title = "String"
    }


emptyDocument : Document
emptyDocument =
    { id = 0
    , identifier = "empty"
    , author_id = 0
    , author_name = ""
    , title = ""
    , content = ""
    , rendered_content = ""
    , attributes = defaultAttributes
    , tags = []
    , children = []
    , parent_id = 0
    , parent_title = "String"
    }


defaultMasterDocument : Document
defaultMasterDocument =
    { id = 0
    , identifier = "nullMasterDocument"
    , author_id = 0
    , author_name = ""
    , title = "Null master document"
    , content = "nothing"
    , rendered_content = "nothing"
    , attributes = defaultAttributes
    , tags = []
    , children = []
    , parent_id = 0
    , parent_title = "String"
    }
