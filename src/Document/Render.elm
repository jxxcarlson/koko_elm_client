module Document.Render exposing (putTextToRender, putTextToRenderWithKey)

import Document.Preprocess exposing (preprocessSource)
import External
import Types exposing (Document, Model, Msg)


putTextToRender : Bool -> List String -> Bool -> Document -> Cmd msg
putTextToRender force idList textNeedsUpdate document =
    External.putTextToRender (External.encodeDocument force idList textNeedsUpdate document)


putTextToRenderWithKey : Int -> Model -> ( Model, Cmd Msg )
putTextToRenderWithKey key model =
    let
        _ =
            Debug.log "key::" key
    in
    if key == 27 then
        -- 27: ESCAPE
        ( model
        , putTextToRender
            True
            model.appState.editRecord.idList
            model.appState.textNeedsUpdate
            model.current_document
        )
    else
        ( model, Cmd.none )
