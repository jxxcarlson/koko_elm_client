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
import Action.User exposing (..)
import Action.Search exposing (..)
import Action.Document exposing (updateDocuments, updateContent)
import Data.Document exposing (documents)
import Request.User exposing (loginUserCmd, getTokenCompleted, registerUserCmd)
import Request.Document exposing (getDocumentsWith)
import Request.Api exposing (loginUrl, registerUserUrl)
import Views.Search exposing (documentSearchForm)
import Time exposing (Time, second)
import Views.External exposing (windowData, windowSetup)


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
        { model | window = new_window, message = "w: " ++ (toString model.window.width) ++ ", h: " ++ (toString model.window.height) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Resize w h ->
            ( (updateWindow model w h), toJs (Views.External.windowData model model.page) )

        GoTo p ->
            ( { model | page = p }, toJs (Views.External.windowData model p) )

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

        SetSearchTerm searchTerms ->
            updateSearch model searchTerms

        DoSearch key ->
            if key == 13 then
                -- 13: RETURN/ENTER
                ( { model | info = "I will search on: " ++ model.searchState.query }
                , getDocumentsWith model.searchState.query
                )
            else
                ( model, Cmd.none )

        DoRender key ->
            if key == 27 then
                -- 27: ESCAPE
                ( { model | info = "ESCAPE pressed, rendering ..." }, render model.current_document.rendered_content )
            else
                ( model, Cmd.none )

        GetDocuments (Ok serverReply) ->
            case (Data.Document.documents serverReply) of
                Ok documentsRecord ->
                    updateDocuments model documentsRecord

                Err _ ->
                    ( { model | info = "Could not decode server reply" }
                    , Cmd.none
                    )

        GetDocuments (Err _) ->
            ( { model | info = "Error on GET: " ++ (toString Err) }, Cmd.none )

        SelectDocument document ->
            ( { model | current_document = document, message = "SelectDocument" }, render document.rendered_content )

        InputContent content ->
            updateContent model content

        Refresh ->
            ( { model | message = "Refresh, rendering" }, render model.current_document.rendered_content )

        UseSearchDomain searchDomain ->
            updateSearchDomain model searchDomain

        Tick time ->
            if model.page == EditorPage then
                ( { model | message = "Tick, rendering" }, render model.current_document.rendered_content )
            else
                ( model, Cmd.none )

        SendToJS str ->
            ( model, toJs str )

        SetupPages ->
            ( model, toJs (Views.External.windowData model model.page) )



-- PORTS


port render : String -> Cmd msg


port toJs : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (30 * Time.second) Tick
        , Window.resizes (\{ width, height } -> Resize width height)
        ]


windowCss model =
    [ Css.width (Css.px ((toFloat model.window.width) - 100.0))
    , Css.height (Css.px (0.9 * (toFloat model.window.height - 575.0)))
    ]


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



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        current_user =
            User "" "" "" "" ""

        registerUser =
            False

        title =
            "Test document"

        text =
            "The correct formula is $$\\int_0^1 x^n dx= \\frac{1}{n+1}$$"

        rendered_text =
            "The correct formula is $$\\int_0^1 x^n dx= \\frac{1}{n+1}$$"

        doc =
            Document 0 0 title text rendered_text

        searchState =
            SearchState "" Public
    in
        ( Model
            (KWindow flags.width flags.height)
            HomePage
            TableOfContents
            "Please sign in"
            current_user
            registerUser
            ""
            ""
            doc
            [ doc ]
            searchState
            True
        , render doc.rendered_content
        )
