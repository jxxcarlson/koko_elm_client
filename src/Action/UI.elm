module Action.UI exposing (..)

-- import Types exposing (Model, Msg, Page, AppState, Tool)

import Array
import Configuration
import External
import Types exposing (..)
import Views.External




displayPage : Model -> Page
displayPage model =
    if model.window.width <= Configuration.phoneWidth
      then
        -- if model.current_document.attributes.docType == ""
        Debug.log "(1) In displayPage, page" Types.ReaderPage
      else if model.appState.page == Types.ReaderPage || model.appState.page == Types.EditorPage  then
        Debug.log "(2) In displayPage, page" model.appState.page
      else
        Debug.log "(3) In displayPage, page" Types.ReaderPage

toggleMenu : String -> Model -> (Model, Cmd Msg)
toggleMenu menu model =
    let
        appState =
            model.appState

        newAppState =
            case menu of
                "Main" ->
                    { appState | menuDropped = (not appState.menuDropped) }

                "textType" ->
                    { appState | textTypeMenuDropped = (not appState.textTypeMenuDropped) }

                "docType" ->
                    { appState | docTypeMenuDropped = (not appState.docTypeMenuDropped) }

                _ ->
                    appState
    in
        ( { model | appState = newAppState }, Cmd.none )


toggleTextMenu : Model -> (Model, Cmd Msg)
toggleTextMenu model =
    let
        appState =
            model.appState

        newAppState =
            { appState | menuDropped = (not appState.menuDropped) }
    in
        ( { model | appState = newAppState }, Cmd.none )

toggleRegister : Model -> (Model, Cmd Msg)
toggleRegister model =
    let
        appState =
            model.appState

        newAppState =
            { appState | registerUser = (not appState.registerUser) }
    in
        ( { model | appState = newAppState }, Cmd.none )


appStateToggleAuthorizing : Model -> AppState
appStateToggleAuthorizing model =
    let
        appState =
            model.appState
    in
        { appState | authorizing = not appState.authorizing }

toggleAuthorizing : Model -> (Model, Cmd Msg)
toggleAuthorizing model =
    let
        oldAppState =
            model.appState

        newAppState =
            { oldAppState | authorizing = (not oldAppState.authorizing), page = Types.HomePage }
    in
        ( { model | appState = newAppState }, External.toJs (Views.External.windowData model Types.HomePage) )

setAuthorizing : Model -> Bool -> (Model, Cmd Msg)
setAuthorizing model value =
    let
        oldAppState =
            model.appState

        newAppState =
            { oldAppState | authorizing = value, page = Types.HomePage }
    in
        ( { model | appState = newAppState }, External.toJs (Views.External.windowData model Types.HomePage) )



appStateWithPage : Model -> Page -> AppState
appStateWithPage model page =
    let
        appState =
            model.appState
    in
        { appState | page = page, tool = updateTool model page }


updateToolStatus : Model -> Tool -> AppState
updateToolStatus model tool =
    let
        appState =
            model.appState
    in
        { appState | tool = tool }




updateTool : Model -> Page -> Tool
updateTool model page =
    let
        currentAppState =
            model.appState

        newTool =
            case page of
                ReaderPage ->
                  TableOfContents

                EditorPage ->
                  EditorTools
                    -- if currentAppState.tool == ReaderTools then
                    --     EditorTools
                    -- else
                    --     currentAppState.tool

                _ ->
                    currentAppState.tool
    in
        newTool



{-

   STRINGS:

-}


queryMessage : SearchDomain -> String
queryMessage domain =
    let
        domain_ =
            case domain of
                Private ->
                    "my documents"

                Public ->
                    "public documents"

                All ->
                  "all documents"
    in
        "search " ++ domain_ ++ " for "


numberOfDocuments : String -> Model -> String
numberOfDocuments title model =
    title ++ ": " ++ (toString (List.length model.documents))



tocNumberOfDocuments : Model -> String
tocNumberOfDocuments model =
    "Contents: " ++ (toString (List.length model.current_document.children)) ++ " documents"


displayIdentifier : Model -> String
displayIdentifier model =
    let
        parts =
            (String.split "." model.current_document.identifier) |> Array.fromList
        datePart = Array.get 2 parts |> Maybe.withDefault "--"
        hashPart = Array.get 3 parts |> Maybe.withDefault "--"

    in
        datePart ++ "." ++ hashPart
