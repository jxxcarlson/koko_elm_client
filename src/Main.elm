port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Window exposing (..)
import Types exposing (..)
import Views.Component exposing (pageSelector)
import Views.Home exposing (home)
import Views.Reader exposing (reader)
import Views.Editor exposing (editor)
import Css exposing (asPairs)
import Utility exposing (styles)
import Action.User exposing (..)
import Action.Search exposing (..)
import Action.Document exposing (updateDocuments, updateContent)
import Data.Document exposing (documents)
import Request.User exposing (loginUserCmd, getTokenCompleted, registerUserCmd)
import Request.Document exposing (getDocumentsWith)
import Request.Api exposing (loginUrl, registerUserUrl)
import Views.Search exposing (documentSearchForm)


-- import JSInterface exposing (toJs)
-- import Koko.Asciidoc exposing (toHtml)

import Koko.Asciidoc exposing (toHtml)


main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


updateWindow : Model -> Int -> Int -> Model
updateWindow model w h =
    let
        new_window =
            KWindow w h
    in
        { model | window = new_window }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Resize w h ->
            ( (updateWindow model w h), Cmd.none )

        GoTo p ->
            ( { model | page = p }, Cmd.none )

        SelectTool t ->
            ( { model | tool = t }, Cmd.none )

        Name name ->
            updateName model name

        Username username ->
            updateUsername model username

        Email email ->
            updateEmail model email

        Password password ->
            updatePassword model password

        Login ->
            ( model, loginUserCmd model loginUrl )

        Register ->
            ( model, registerUserCmd model registerUserUrl )

        GetTokenCompleted result ->
            getTokenCompleted model result

        Signout ->
            signout model

        ToggleRegister ->
            ( { model | registerUser = not model.registerUser }, Cmd.none )

        SendToJs str ->
            ( model, toJs str )

        UpdateStr str ->
            ( { model | info = str }, Cmd.none )

        SetSearchTerm searchTerms ->
            updateSearch model searchTerms

        KeyUp key ->
            if key == 13 then
                ( { model | info = "I will search on: " ++ model.searchTerms }
                , getDocumentsWith model.searchTerms
                )
            else
                ( model, Cmd.none )

        GetDocuments (Ok serverReply) ->
            case (Data.Document.documents serverReply) of
                Ok documentsRecord ->
                    updateDocuments model documentsRecord

                Err _ ->
                    ( { model | info = "Could not decode server reply" }, Cmd.none )

        GetDocuments (Err _) ->
            ( { model | info = "Error on GET: " ++ (toString Err) }, Cmd.none )

        SelectDocument document ->
            ( { model | current_document = document }, Cmd.none )

        InputContent content ->
            updateContent model content


port toJs : String -> Cmd msg


port toElm : (String -> msg) -> Sub msg


port render : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\{ width, height } -> Resize width height)


subscriptions2 : Model -> Sub Msg
subscriptions2 model =
    toElm UpdateStr


windowCss model =
    [ Css.width (Css.px ((toFloat model.window.width) - 100.0))
    , Css.height (Css.px (0.9 * (toFloat model.window.height - 575.0)))
    ]



-- windowCss model =
--     [ Css.width (Css.px ((toFloat model.window.width) - 100.0))
--     , Css.height (Css.px (0.9 * (toFloat model.window.height - 575.0)))
--     ]


page : Model -> Html Msg
page model =
    case model.page of
        ReaderPage ->
            reader model

        EditorPage ->
            editor model

        HomePage ->
            home model


view : Model -> Html Msg
view model =
    --
    div []
        [ div [ id "header" ]
            [ span [] [ text "Noteshare" ]
            , Views.Component.pageSelector model
            , Views.Search.documentSearchForm model
            ]
        , (page model)
        , div [ id "footer" ]
            [ span [ id "message" ] [ text model.message ]
            , span [ id "info" ] [ text model.info ]
            ]
        ]
