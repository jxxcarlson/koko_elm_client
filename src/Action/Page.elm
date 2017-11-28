module Action.Page exposing (..)

import Types exposing (..)
import User.Login
import Action.UI
import Document.Dictionary
import Action.Document
import External
import MiniLatex.Differ as Differ exposing (EditRecord)
import Request.Document
import Views.External
import Time
import Task
import Request.Document


setHomePage model =
    let
        query =
            "key=home&authorname=" ++ (User.Login.shortUsername model)

        cmd =
            Request.Document.getDocuments "public/documents" query GetDocuments model.current_user.token

        appState =
            model.appState

        newAppState =
            { appState | page = ReaderPage, activeDocumentList = DocumentStackList }
    in
        ( { model | appState = newAppState }, cmd )


{-| NOTE:

I am disabling the line

    (\appState -> { appState | textBuffer = model.current_document.content })

for the moment. I believe that it is what is causing overwrites.

-}
setEditPage model =
    let
        appState =
            model.appState

        newAppState =
            { appState
                | page = EditorPage
                , tool = Action.UI.updateTool model EditorPage
                , textBuffer = model.current_document.content
                , textBufferDirty = False
            }
                |> Action.Document.clearEditRecord
    in
        ( { model | appState = newAppState }
        , Cmd.batch
            [ External.toJs (Views.External.windowData model EditorPage)
            , Task.perform ReceiveTime Time.now
            , Document.Dictionary.setItemInDict ("title=texmacros&authorname=" ++ model.current_user.username) "texmacros" model.current_user.token
            ]
        )


goToPage : Page -> Model -> ( Model, Cmd Msg )
goToPage p model =
    case ( p, model.appState.signedIn ) of
        ( EditorPage, True ) ->
            setEditPage model

        ( EditorPage, False ) ->
            ( { model
                | appState = Action.UI.appStateWithPage model StartPage
                , message = "Please sign in if you wish to edit"
              }
            , External.toJs (Views.External.windowData model p)
            )

        ( StartPage, True ) ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = StartPage, masterDocLoaded = False }
            in
                ( { model | appState = newAppState }
                , Cmd.batch
                    [ Request.Document.getDocumentWithAuthenticatedQuery GetSpecialDocument model.current_user.token "key=sidebarNotes"
                    , Document.Dictionary.setItemInDict "key=sidebarNotes" "sidebar" model.current_user.token
                    ]
                )

        ( StartPage, False ) ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = StartPage, masterDocLoaded = False }
            in
                ( { model | appState = newAppState }
                , Cmd.batch
                    [ Request.Document.getDocumentWithQuery GetSpecialDocument "2017-8-26@18-1-42.887330"
                    , Document.Dictionary.setItemInDict "ident=2017-8-26@18-1-42.887330" "welcome" model.current_user.token
                    ]
                )

        ( ReaderPage, True ) ->
            ( { model | appState = Action.UI.appStateWithPage model p }
            , Request.Document.getDocumentWithAuthenticatedQuery GetSpecialDocument model.current_user.token "key=sidebarNotes"
            )

        ( ReaderPage, False ) ->
            ( { model | appState = Action.UI.appStateWithPage model p }
            , Request.Document.getDocumentWithQuery GetSpecialDocument "ident=2017-8-4@22-21-10.03ed17"
            )

        ( _, _ ) ->
            ( { model | appState = Action.UI.appStateWithPage model p }, External.toJs (Views.External.windowData model p) )
