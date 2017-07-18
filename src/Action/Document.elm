module Action.Document exposing (..)

import Types exposing (..)
import Utility exposing (replaceIf)
import Request.Document exposing (putDocument)
import External exposing (render)
import Action.UI exposing (displayPage, updateToolStatus, appStateWithPage)
import Views.External exposing (windowData)
import External exposing (toJs)
import Utility
import Action.Search
import Regex
import LatexParser.Render


updateCurrentDocumentWithContent : String -> Model -> ( Model, Cmd Msg )
updateCurrentDocumentWithContent content model =
    let
        oldDocument =
            model.current_document

        -- TEST: foobar = Debug.log "foo" model.current_document.id
        newDocument =
            { oldDocument
                | content = content
                , rendered_content = preprocess content oldDocument
            }

        newModel =
            { model | message = preprocess content oldDocument }
    in
        updateCurrentDocument newModel newDocument


preprocess : String -> Document -> String
preprocess content document =
    if document.attributes.docType == "master" then
        preprocessMaster content
    else if document.attributes.textType == "latex" then
        preprocessLatex content
    else
        content


preprocessMaster : String -> String
preprocessMaster content =
    (String.split "TOC:\n" content) |> List.head |> Maybe.withDefault ""


replace : String -> String -> String -> String
replace search substitution string =
    string
        |> Regex.replace Regex.All (Regex.regex (Regex.escape search)) (\_ -> substitution)


preprocessLatex : String -> String
preprocessLatex content =
    content
        |> LatexParser.Render.transformText



-- |> Regex.replace Regex.All (Regex.regex "%.*") (\_ -> "")
-- |> LatexParser.Render.transformText.identity
-- |> Regex.replace Regex.All (Regex.regex "\\label{.*}") (\_ -> "")
-- |> Regex.replace Regex.All (Regex.regex "\\emph{(.*)}") (\{match} -> "<it>\\1</it>")


setTextType : String -> Model -> ( Model, Cmd Msg )
setTextType textType model =
    let
        oldDocument =
            model.current_document

        docAttributes =
            oldDocument.attributes

        newDocumentAttributes =
            { docAttributes | textType = textType }

        appState =
            model.appState

        newAppState =
            { appState | textTypeMenuDropped = False }

        newModel =
            { model | appState = newAppState }

        -- TEST: foobar = Debug.log "foo" model.current_document.id
        newDocument =
            { oldDocument | attributes = newDocumentAttributes }
    in
        updateCurrentDocument newModel newDocument


setDocType : String -> Model -> ( Model, Cmd Msg )
setDocType docType model =
    let
        oldDocument =
            model.current_document

        docAttributes =
            oldDocument.attributes

        newDocAttributes =
            { docAttributes | docType = docType }

        appState =
            model.appState

        newAppState =
            { appState | docTypeMenuDropped = False }

        newModel =
            { model | appState = newAppState }

        -- TEST: foobar = Debug.log "foo" model.current_document.id
        newDocument =
            { oldDocument | attributes = newDocAttributes }
    in
        updateCurrentDocument newModel newDocument


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

        masterDocLoaded =
            if current_document.attributes.docType == "master" then
                True
            else
                False

        updatedAppState =
            { appState | page = page, tool = TableOfContents, masterDocLoaded = masterDocLoaded }
    in
        ( { model
            | documents = documentsRecord.documents
            , current_document = current_document
            , appState = updatedAppState
            , counter = Debug.log "updateDocuments" (model.counter + 1)
          }
        , Cmd.batch
            [ toJs (windowData model model.appState.page)
            , renderDocument current_document
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
        , Cmd.batch [ putDocument model document, renderDocument document ]
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
    ( { model | appState = updateToolStatus model EditorTools }
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
            , message =
                "Selected: "
                    ++ (toString document.id)
                    ++ " ("
                    ++ document.title
                    ++ ")"
                    ++ " -- "
                    ++ (toString (List.length document.children))
                    ++ " children"
            , counter = model.counter + 1
          }
        , Cmd.batch
            [ toJs (windowData model (displayPage model))
            , renderDocument document
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
    , renderDocument document
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


doSearch : SearchDomain -> Int -> Model -> ( Model, Cmd Msg )
doSearch searchDomain key model =
    if (Debug.log "key" key) == 13 then
        let
            newSearchState =
                Action.Search.updatedSearchState model searchDomain

            updatedModel =
                { model
                    | searchState = newSearchState
                    , message = (Action.UI.queryMessage searchDomain) ++ Utility.queryText model.searchState.query
                    , appState = Action.UI.updateToolStatus model TableOfContents
                    , documents2 = model.documents
                }
        in
            ( { updatedModel | appState = Action.UI.appStateWithPage model (Action.UI.displayPage model), info = "tool: " ++ (toString updatedModel.appState.tool) }
            , Cmd.batch
                [ Request.Document.getDocumentsWith newSearchState model.current_user.token
                , renderDocument model.current_document
                ]
            )
    else
        ( model, Cmd.none )


selectMasterDocument : Document -> Model -> ( Model, Cmd Msg )
selectMasterDocument document model =
    if document.attributes.docType == "master" then
        selectMasterDocumentAux document model
    else
        ( model, Cmd.none )


selectMasterDocumentAux : Document -> Model -> ( Model, Cmd Msg )
selectMasterDocumentAux document model =
    let
        searchState =
            model.searchState

        updatedSearchState =
            { searchState | query = "master=" ++ (toString document.id) }

        updatedModel =
            { model | searchState = updatedSearchState }
    in
        doSearch model.searchState.domain 13 updatedModel


renderDocument document =
    External.render (External.encodeDocument document)
