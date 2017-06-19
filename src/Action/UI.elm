module Action.UI exposing (..)

import Types exposing (Model, Msg, Page)


displayPage : Model -> Page
displayPage model =
    if model.page == Types.HomePage then
        Types.ReaderPage
    else
        model.page


toggleMenu model =
    let
        appState =
            model.appState

        newAppState =
            { appState | menuDropped = (not appState.menuDropped) }
    in
        ( { model | appState = newAppState }, Cmd.none )
