module Action.UI exposing (..)

import Types exposing (Model, Msg, Page, AppState, Tool)


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


toggleRegister model =
    let
        appState =
            model.appState

        newAppState =
            { appState | registerUser = (not appState.registerUser) }
    in
        ( { model | appState = newAppState }, Cmd.none )


updateToolStatus : Model -> Tool -> AppState
updateToolStatus model tool =
    let
        appState =
            model.appState
    in
        { appState | tool = tool }
