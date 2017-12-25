module Update.UI exposing (update)

import Action.Document
import Action.UI
import Types exposing (Msg(UIMsg), UIMsg(..))
import Views.TOC


update submessage model =
    case submessage of
        ToggleListView ->
            Views.TOC.toggleListView model

        ToggleUpdateRate ->
            ( Action.Document.toggleUpdateRate model, Cmd.none )

        ToggleMenu menu ->
            Action.UI.toggleMenu menu model

        Message str ->
            ( { model | message = str }, Cmd.none )

        SelectTool t ->
            ( { model | appState = Action.UI.updateToolStatus model t }, Cmd.none )
