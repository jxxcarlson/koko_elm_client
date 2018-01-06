module Document.Document
    exposing
        ( blankDocument
        , defaultDocument
        , defaultMasterDocument
        , diaryEntry
        , emptyDocument
        , errorDocument
        , hasTag
        , pageNotFoundDocument
        , startDocument
        )

import Date exposing (Date, day, dayOfWeek, month, year)
import Date.Extra
import Types exposing (Document, DocumentAttributes)


hasTag : String -> Document -> Bool
hasTag tagg document =
    List.any (\x -> x == tagg) document.tags


defaultAttributes : DocumentAttributes
defaultAttributes =
    DocumentAttributes False "adoc" "standard" 0 Nothing


diaryEntry : Maybe Date -> Document
diaryEntry maybeDate =
    case maybeDate of
        Just date ->
            realDiaryEntry date

        Nothing ->
            blankDocument


realDiaryEntry : Date -> Document
realDiaryEntry date =
    { id = 0
    , identifier = "nullDocument"
    , author_id = 0
    , author_name = ""
    , title = Date.Extra.toFormattedString "EEE MMM d, y" date
    , content = "New diary entry"
    , rendered_content = "New diary entry"
    , attributes = defaultAttributes
    , tags = [ "diary" ]
    , children = []
    , parent_id = 0
    , parent_title = "String"
    }


defaultDocument : Document
defaultDocument =
    template "Welcome" "Welcome to noteshare"


errorDocument : Document
errorDocument =
    template "Error" "Oops - something went wrong"


pageNotFoundDocument : Document
pageNotFoundDocument =
    template "Page not found" "Sorry, page not found"


startDocument : Document
startDocument =
    template "Welcome" "Welcome to noteshare!"


blankDocument : Document
blankDocument =
    template "New Document" "Write content here"


defaultMasterDocument : Document
defaultMasterDocument =
    template "Null master document" "nothing"


emptyDocument : Document
emptyDocument =
    { id = 0
    , identifier = "-"
    , author_id = 0
    , author_name = ""
    , title = ""
    , content = ""
    , rendered_content = ""
    , attributes = defaultAttributes
    , tags = []
    , children = []
    , parent_id = 0
    , parent_title = "-"
    }


template : String -> String -> Document
template title content =
    let
        doc =
            emptyDocument
    in
        { doc | title = title, content = content, rendered_content = content }
