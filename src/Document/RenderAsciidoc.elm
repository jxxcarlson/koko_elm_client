module Document.RenderAsciidoc exposing(put, putWithKey)

import External
import Types exposing(Model, Document, Msg)
import Document.Preprocess exposing(preprocessSource)

put : Document -> Cmd msg
put document =
  let
     document2 = {document | content = preprocessSource document.content}
  in
     External.render (External.encodeDocument document2)

putWithKey : Int -> Model -> (Model, Cmd Msg)
putWithKey key model =
  if key == 27 then
      -- 27: ESCAPE
      ( { model | info = "ESCAPE pressed, rendering ..." }
      , put model.current_document
      )
  else
      ( model, Cmd.none )
