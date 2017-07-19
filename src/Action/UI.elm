module Action.UI exposing (..)

-- import Types exposing (Model, Msg, Page, AppState, Tool)

import Types exposing (..)
import External
import Views.External
import Array


displayPage : Model -> Page
displayPage model =
    if model.appState.page == Types.HomePage then
        Types.ReaderPage
    else
        model.appState.page


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


toggleTextMenu model =
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


appStateToggleAuthorizing : Model -> AppState
appStateToggleAuthorizing model =
    let
        appState =
            model.appState
    in
        { appState | authorizing = not appState.authorizing }


toggleAuthorizing model =
    let
        oldAppState =
            model.appState

        newAppState =
            { oldAppState | authorizing = (not oldAppState.authorizing), page = Types.HomePage }
    in
        ( { model | appState = newAppState }, External.toJs (Views.External.windowData model Types.HomePage) )


setAuthorizing model value =
    let
        oldAppState =
            model.appState

        newAppState =
            { oldAppState | authorizing = value, page = Types.HomePage }
    in
        ( { model | appState = newAppState }, External.toJs (Views.External.windowData model Types.HomePage) )


updateToolStatus : Model -> Tool -> AppState
updateToolStatus model tool =
    let
        appState =
            model.appState
    in
        { appState | tool = tool }

goToPage : Page -> Model -> (Model, Cmd Msg)
goToPage p model =
  if p == EditorPage && model.current_user.token == "" then
      ( { model
          | appState = appStateWithPage model HomePage
          , message = "Please sign in if you wish to edit"
        }
      , External.toJs (Views.External.windowData model p)
      )
  else
      ( { model | appState = appStateWithPage model p }, External.toJs (Views.External.windowData model p) )


appStateWithPage : Model -> Page -> AppState
appStateWithPage model page =
    let
        appState =
            model.appState
    in
        { appState | page = page, tool = updateTool model page }


updateTool : Model -> Page -> Tool
updateTool model page =
    let
        currentAppState =
            model.appState

        newTool =
            case page of
                ReaderPage ->
                    if currentAppState.tool == EditorTools then
                        ReaderTools
                    else
                        currentAppState.tool

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
    in
        "search " ++ domain_ ++ " for "


numberOfDocuments : Model -> String
numberOfDocuments model =
    case model.searchState.domain of
        Private ->
            "My documents: " ++ (toString (List.length model.documents))

        Public ->
            "Public documents: " ++ (toString (List.length model.documents))


tocNumberOfDocuments : Model -> String
tocNumberOfDocuments model =
    "Contents: " ++ (toString (List.length model.current_document.children)) ++ " documents"


displayIdentifier : Model -> String
displayIdentifier model =
    let
        parts =
            (String.split "." model.current_document.identifier) |> Array.fromList
    in
        Array.get 3 parts |> Maybe.withDefault "--"
