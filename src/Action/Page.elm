module Action.Page exposing (..)

import Action.Document
import Action.UI
import Document.Dictionary
import Request.Document
import Task
import Time
import Types exposing (Model, Page(..), Msg(DocMsg, PeriodicMsg), PeriodicMsg(ReceiveTime), DocMsg(..), 
   ActiveDocumentList(..), InfoForOutside(WindowData))
import User.Login
import Views.External
import OutsideInfo

setHomePage : Model -> (Model, Cmd Msg)
setHomePage model =
    let
        route = "public/documents" ++ "key=home&authorname=" ++ User.Login.shortUsername model

        cmd =
            Request.Document.getDocumentsNew route model.current_user.token (DocMsg << GetDocuments)

        appState =
            model.appState

        newAppState =
            { appState | page = ReaderPage, activeDocumentList = DocumentStackList }
    in
    ( { model | appState = newAppState }, cmd )


setEditPage : Model -> (Model, Cmd Msg)
setEditPage model =
    let
        appState =
            model.appState

        lastEditTime =
            model.time

        newAppState =
            { appState
                | page = EditorPage
                , tool = Action.UI.updateTool model EditorPage
                , textNeedsUpdate = False
            }
                |> Action.Document.clearEditRecord
    in
    ( { model | appState = newAppState, lastEditTime = lastEditTime }
    , Cmd.batch
        [ OutsideInfo.sendInfoOutside (WindowData <| Views.External.encodeWindowData model EditorPage)
        , Task.perform (PeriodicMsg << ReceiveTime) Time.now
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
            , OutsideInfo.sendInfoOutside (WindowData <| Views.External.encodeWindowData model p)
            )

        (ImagePage, True) ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = ImagePage, masterDocLoaded = False }
            in
            ( { model | appState = newAppState }, Cmd.none )

        ( StartPage, True ) ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = StartPage, masterDocLoaded = False }
            in
            ( { model | appState = newAppState }
            , Cmd.batch
                [ Request.Document.getDocumentWithAuthenticatedQuery (DocMsg << GetSpecialDocument) model.current_user.token "key=sidebarNotes"
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
                [ Request.Document.getDocumentWithQuery (DocMsg << GetSpecialDocument) "2017-8-26@18-1-42.887330"
                , Document.Dictionary.setItemInDict "ident=2017-8-26@18-1-42.887330" "welcome" model.current_user.token
                ]
            )

        ( ReaderPage, True ) ->
            ( { model | appState = Action.UI.appStateWithPage model p }
            , Cmd.batch
                [ Request.Document.getDocumentWithAuthenticatedQuery (DocMsg << GetSpecialDocument) model.current_user.token "key=sidebarNotes"
                , Document.Dictionary.setItemInDict "key=sidebarNotes" "sidebar" model.current_user.token
                ]
            )

        ( ReaderPage, False ) ->
            ( { model | appState = Action.UI.appStateWithPage model p }
            , Request.Document.getDocumentWithQuery (DocMsg << GetSpecialDocument) "ident=2017-8-4@22-21-10.03ed17"
            )

        ( _, _ ) ->
            ( { model | appState = Action.UI.appStateWithPage model p }, OutsideInfo.sendInfoOutside (WindowData <| Views.External.encodeWindowData model p))
