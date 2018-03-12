module Update.Page exposing (update)

import Action.Page
import Document.Search
import External
import Nav.Navigation
import Request.Document
import Types exposing (DocMsg(..), Msg(DocMsg), Page(..), PageMsg(..), SearchDomain(..), SearchOrder(..), InfoForOutside(WindowData))
import User.Display
import User.Request
import Views.External
import OutsideInfo


update submessage model =
    case submessage of
        UserHomePage ->
            Action.Page.setHomePage model

        GetPublicPage searchTerm ->
            Document.Search.searchWithParameters searchTerm Alphabetical Public ReaderPage model

        InitStartPage ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = StartPage, masterDocLoaded = False, authorizing = False }
            in
            ( { model | appState = newAppState }
            , Request.Document.getDocumentWithQuery (DocMsg << GetSpecialDocument) "ident=2017-8-26@18-1-42.887330"
            )

        GotoUserHomePages ->
            User.Display.goToUserHomePages model

        GotoUserPreferencesPage ->
            let
                appState =
                    model.appState

                newAppState =
                    { appState | page = UserPreferencesPage }
            in
            ( { model
                | appState = newAppState
                , textInputBuffer = model.current_user.blurb
              }
            , User.Request.get model.current_user.id
            )

        GetHomePageForUserHomePages searchTerm username ->
            let
                model2 =
                    { model | selectedUserName = username }

                ( newModel, cmd ) =
                    Document.Search.searchWithParameters searchTerm Alphabetical Public UserHomePages model2
            in
            ( newModel, Cmd.batch [ cmd ] )

        GoTo p ->
            Action.Page.goToPage p model

        SetupPages ->
            ( model, OutsideInfo.sendInfoOutside (WindowData <| Views.External.encodeWindowData model Types.StartPage))
            
        GoToPage maybepage ->
            Nav.Navigation.navigateTo maybepage model
