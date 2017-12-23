module Update.Auth exposing (update)

import Action.UI
import Request.Api
import Types exposing (AuthMsg(..))
import User.Auth
import User.Login


update submessage model =
    case submessage of
        Name name ->
            User.Login.updateName model name

        Username username ->
            User.Login.updateUsername model username

        Password password ->
            User.Login.updatePassword model password

        Signout ->
            User.Login.signout "Please sign in" model

        AuthenticationAction ->
            if model.appState.signedIn then
                User.Login.signout "You are now signed out." model
            else
                Action.UI.setAuthorizing model True

        CancelAuthentication ->
            Action.UI.toggleAuthorizing model

        Login ->
            User.Login.doLogin model

        Register ->
            ( model, User.Auth.registerUserCmd model Request.Api.registerUserUrl )

        CompleteRegistration result ->
            User.Login.completeRegistration result model

        GetTokenCompleted result ->
            User.Auth.getTokenCompleted model result

        SignOutOrIn ->
            User.Login.signOutOrIn model

        ToggleRegister ->
            Action.UI.toggleRegister model
