module Action.Page exposing(..)

import Types exposing (..)
import Action.UI
import External
import Views.External
import Action.Document
import Time
import Task

goToPage : Page -> Model -> (Model, Cmd Msg)
goToPage p model =
    if p == EditorPage then
      ( { model | appState = Action.UI.appStateWithPage model p },
         Cmd.batch[
           External.toJs (Views.External.windowData model p)
           , Task.perform ReceiveTime Time.now
        ]
      )
    else if p == EditorPage && model.current_user.token == "" then
        ( { model
            | appState = Action.UI.appStateWithPage model HomePage
            , message = "Please sign in if you wish to edit"
          }
        , External.toJs (Views.External.windowData model p)
        )
    else if p == HomePage && model.current_user.token /= "" then
          Action.Document.search Private "sort=updated&limit=12" HomePage model
    else
        ( { model | appState = Action.UI.appStateWithPage model p }, External.toJs (Views.External.windowData model p) )
