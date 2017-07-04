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

setTextType : String -> Model -> ( Model, Cmd Msg )
setTextType textType model =
    let
        oldDocument =
            model.current_document

        attributes = oldDocument.attributes

        newAttributes = {attributes | textType = textType}

        -- TEST: foobar = Debug.log "foo" model.current_document.id
        newDocument =
            { oldDocument | attributes = newAttributes }
    in
        updateCurrentDocument model newDocument


parseTagString : String -> List String
parseTagString str =
    String.split "," str
        |> List.map String.trim


updateTags : String -> Model -> ( Model, Cmd Msg )
updateTags tagText model =
    let
        updatedTags =
            parseTagString tagText

        document =
            model.current_document

        updatedDocument =
            { document | tags = updatedTags }
    in
        ( { model | current_document = updatedDocument }, Cmd.none )


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

        appState =
            model.appState

        updatedAppState =
            { appState | page = page, tool = TableOfContents }
    in
        ( { model
            | documents = documentsRecord.documents
            , current_document = current_document
            , appState = updatedAppState
            , counter = Debug.log "updateDocuments" (model.counter + 1)
          }
        , Cmd.batch
            [ toJs (windowData model model.appState.page)
            , render (External.encodeDocument current_document)
            ]

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
            , message = "!! Rendering #" ++ (toString document.id)
          }
        , Cmd.batch [ putDocument model document, External.render (External.encodeDocument document)]
        )


saveCurrentDocument : Model -> ( Model, Cmd Msg )
saveCurrentDocument model =
    ( { model | message = ("Saved document " ++ (toString model.current_document.id)) }, putDocument model model.current_document )


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
    let
        appState =
            model.appState

        newAppState =
            { appState | textBuffer = document.content, textBufferDirty = False }
    in
        ( { model
            | current_document = document
            , appState = newAppState
            , message = "Selected: " ++ document.title
            , counter = model.counter + 1
          }
        , Cmd.batch
            [ toJs (windowData model (displayPage model))
            , render (External.encodeDocument document)
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
    , render (External.encodeDocument document)
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
