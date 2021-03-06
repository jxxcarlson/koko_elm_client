module Document.Stack exposing (push, top, sorted)

import Document.Document
import List.Extra
import Types exposing (Document, DocumentStack)
import Utility


{-| Push a new document onto the stack after deleting
the previous version if it is there.
-}
push : Document -> DocumentStack -> DocumentStack
push document stack =
    let
        _ =
            Debug.log "Pushing document" document.id

        _ =
            Debug.log "Pushed doc tags" document.tags

        newStackPart =
            if List.member "home" document.tags then
                []
            else
                [ document ]

        stack2 =
            Utility.removeWhen (\doc -> doc.id == document.id) stack
    in
        newStackPart ++ (List.take 9 stack2)


sorted : DocumentStack -> DocumentStack
sorted stack =
    List.sortWith lastViewedCompare stack


titleCompare : Document -> Document -> Order
titleCompare doc1 doc2 =
    case ( doc1.title == doc2.title, doc1.title < doc2.title ) of
        ( True, _ ) ->
            EQ

        ( _, True ) ->
            LT

        ( _, False ) ->
            GT


lastViewedCompare : Document -> Document -> Order
lastViewedCompare doc1 doc2 =
    let
        t1 =
            case doc1.attributes.lastViewed of
                Just time ->
                    time

                Nothing ->
                    0

        t2 =
            case doc2.attributes.lastViewed of
                Just time ->
                    time

                Nothing ->
                    0
    in
        case ( t1 == t2, t1 < t2 ) of
            ( True, _ ) ->
                EQ

            ( _, True ) ->
                LT

            ( _, False ) ->
                GT


top : Int -> DocumentStack -> Document
top k docstack =
    List.Extra.getAt k docstack |> Maybe.withDefault Document.Document.pageNotFoundDocument
