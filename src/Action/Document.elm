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
    in
        ( { model
            | documents = documentsRecord.documents
            , current_document = current_document
            , info = (toString (List.length model.documents)) ++ " documents found"
          }
        , Cmd.none
        )
