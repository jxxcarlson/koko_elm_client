module Action.Document exposing (..)

import Types exposing (..)
import Utility exposing (replaceIf)
import Request.Document exposing (putDocument)
import External exposing (render)
import Action.UI exposing (displayPage, updateToolStatus, appStateWithPage)
import Views.External exposing (windowData)
import External exposing (toJs)
import Utility


updateCurrentDocumentWithContent : String -> Model -> ( Model, Cmd Msg )
updateCurrentDocumentWithContent content model =
    let
        oldDocument =
            model.current_document

        -- TEST: foobar = Debug.log "foo" model.current_document.id
        newDocument =
            { oldDocument | content = content, rendered_content = content }
    in
        updateCurrentDocument model newDocument


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
          }
        , Cmd.batch
            [ toJs (windowData model model.appState.page)
            , render current_document.rendered_content
            ]
          -- render model.current_document.rendered_text2
        )


updateCurrentDocument : Model -> Document -> ( Model, Cmd Msg )
updateCurrentDocument model document =
    let
        old_documents =
            model.documents

        new_documents =
            Utility.replaceIf (hasId document.id) document old_documents

        appState =
            model.appState

        newAppState =
            { appState | textBufferDirty = False }
    in
        ( { model
            | current_document = document
            , documents = new_documents
            , appState = newAppState
          }
        , Cmd.batch [ putDocument model document, External.render document.rendered_content ]
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
    ( { model | appState = updateToolStatus model DocumentParameterTools }
    , Request.Document.createDocument document model.current_user.token
    )


selectDocument : Model -> Document -> ( Model, Cmd Msg )
selectDocument model document =
    ( { model
        | current_document = document
        , message = "Selected: " ++ document.title
        , counter = model.counter + 1
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
        , counter = model.counter + 1
      }
    , render document.rendered_content
    )


togglePublic : Model -> ( Model, Cmd Msg )
togglePublic model =
    let
        document =
            model.current_document

        attributes =
            document.attributes

        newAttributes =
            { attributes | public = not attributes.public }

        newDocument =
            { document | attributes = newAttributes }

        updatedModel =
            { model | current_document = newDocument }
    in
        updateCurrentDocument updatedModel newDocument
