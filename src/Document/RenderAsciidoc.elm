module Document.RenderAsciidoc exposing (put, putWithKey)

import External
import Types exposing (Model, Document, Msg)
import Document.Preprocess exposing (preprocessSource)


put : Bool -> Document -> Cmd msg
put textBufferDirty document =
    let
        -- document2 =
        --     { document | content = preprocessSource document.content }
        --
        _ =
            Debug.log "RenderAsciidoc.put" document.title
    in
        External.putTextToRender (External.encodeDocument textBufferDirty document)


putWithKey : Int -> Model -> ( Model, Cmd Msg )
putWithKey key model =
    if key == 27 then
        -- 27: ESCAPE
        ( model, put model.appState.textBufferDirty model.current_document )
    else
        ( model, Cmd.none )
