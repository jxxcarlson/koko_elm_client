module Action.Page exposing (..)

import Types exposing (..)
import Action.UI
import External
import Views.External
import Time
import Task
import Request.Document


goToPage : Page -> Model -> ( Model, Cmd Msg )
goToPage p model =
    case ( p, model.appState.signedIn ) of
        ( EditorPage, True ) ->
            ( { model | appState = Action.UI.appStateWithPage model p }
            , Cmd.batch
                [ External.toJs (Views.External.windowData model p)
                , Task.perform ReceiveTime Time.now
                ]
            )

        ( EditorPage, False ) ->
            ( { model
                | appState = Action.UI.appStateWithPage model HomePage
                , message = "Please sign in if you wish to edit"
              }
            , External.toJs (Views.External.windowData model p)
            )

        ( HomePage, True ) ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = HomePage, masterDocLoaded = False }
            in
                ( { model | appState = newAppState }
                , Request.Document.getSpecialDocumentWithAuthenticatedQuery model.current_user.token "key=sidebarNotes"
                )

        ( HomePage, False ) ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = HomePage, masterDocLoaded = False }
            in
                ( { model | appState = newAppState }
                , Request.Document.getSpecialDocumentWithQuery "2017-8-26@18-1-42.887330"
                )

        ( ReaderPage, True ) ->
            ( { model | appState = Action.UI.appStateWithPage model p }
            , Request.Document.getSpecialDocumentWithAuthenticatedQuery model.current_user.token "key=sidebarNotes"
            )

        ( ReaderPage, False ) ->
            ( { model | appState = Action.UI.appStateWithPage model p }
            , Request.Document.getSpecialDocumentWithQuery "ident=2017-8-4@22-21-10.03ed17"
            )

        ( _, _ ) ->
            ( { model | appState = Action.UI.appStateWithPage model p }, External.toJs (Views.External.windowData model p) )
