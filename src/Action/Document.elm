module Action.Document exposing (..)

import Types exposing (..)


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
            if model.page == HomePage then
                ReaderPage
            else
                model.page
    in
        ( { model
            | documents = documentsRecord.documents
            , current_document = current_document
            , page = page
            , info = (toString (List.length documentsRecord.documents)) ++ " documents found"
          }
        , Cmd.none
        )
