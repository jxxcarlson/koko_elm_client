module Document.Render exposing (put, putWithKey)

import External
import Types exposing (Model, Document, Msg)
import Document.Preprocess exposing (preprocessSource)


put : Bool -> List String -> Bool -> Document -> Cmd msg
put force idList textBufferDirty document =
    External.putTextToRender (External.encodeDocument force idList textBufferDirty document)


putWithKey : Int -> Model -> ( Model, Cmd Msg )
putWithKey key model =
    if key == 27 then
        -- 27: ESCAPE
        ( model, put True model.appState.editRecord.idList model.appState.textBufferDirty model.current_document )
    else
        ( model, Cmd.none )
