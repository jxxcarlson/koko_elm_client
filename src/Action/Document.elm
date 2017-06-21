module Action.Document exposing (..)

import Types exposing (..)
import Utility exposing (replaceIf)
import Request.Document exposing (putCurrentDocument)
import External exposing (render)
import Action.UI exposing (displayPage, updateToolStatus, appStateWithPage)
import Views.External exposing (windowData)
import External exposing (toJs)
import Utility


updateDocuments : Model -> DocumentsRecord -> ( Model, Cmd Msg )
updateDocuments model documentsRecord =
    let
        current_document =
            case List.head documentsRecord.documents of
                Just document ->
                    document

                Nothing ->
                    defaultDocument

        page =
            if model.appState.page == HomePage then
                ReaderPage
            else
                model.appState.page

        tool =
            TableOfContents
    in
        ( { model
            | documents = documentsRecord.documents
            , current_document = current_document
            , appState = appStateWithPage model page
            , appState = updateToolStatus model TableOfContents
            , info = (toString (List.length documentsRecord.documents)) ++ " documents found"
          }
        , Cmd.batch
            [ toJs (windowData model model.appState.page)
            , render current_document.rendered_content
            ]
          -- render model.current_document.rendered_text2
        )


updateContent : Model -> String -> ( Model, Cmd Msg )
updateContent model content =
    let
        docInfo =
            case (getDocumentById model model.current_document.id) of
                Just theDoc ->
                    "Updating doc, id = " ++ (toString theDoc.id)

                Nothing ->
                    "Cannot get doc_info"

        old_document =
            model.current_document

        new_document =
            { old_document | content = content, rendered_content = content }

        -- (a -> Bool) -> a -> List a -> List a
        old_documents =
            model.documents

        new_documents =
            Utility.replaceIf (hasId new_document.id) new_document old_documents
    in
        ( { model
            | current_document = new_document
            , documents = new_documents
            , info =
                docInfo
                -- , documents = new_documents
          }
        , putCurrentDocument model
        )


hasId : Int -> Document -> Bool
hasId id document =
    document.id == id


wordCount : Document -> Int
wordCount document =
    document.content
        |> String.split (" ")
        |> List.length


getDocumentsById : Model -> Int -> List Document
getDocumentsById model k =
    List.filter (\doc -> doc.id == k) model.documents


getDocumentById : Model -> Int -> Maybe Document
getDocumentById model k =
    List.head (getDocumentsById model k)


createDocument : Model -> Document -> ( Model, Cmd Msg )
createDocument model document =
    ( model, Request.Document.createDocument document model.current_user.token )


selectDocument : Model -> Document -> ( Model, Cmd Msg )
selectDocument model document =
    ( { model
        | current_document = document
        , message = "Selected: " ++ document.title
      }
    , Cmd.batch
        [ toJs (windowData model (displayPage model))
        , render document.rendered_content
        ]
    )


selectNewDocument : Model -> Document -> ( Model, Cmd Msg )
selectNewDocument model document =
    ( { model
        | current_document = document
        , documents = [ document ] ++ model.documents
        , message = "New document added: " ++ document.title
        , info = "New document added: " ++ document.title
      }
    , render document.rendered_content
    )
