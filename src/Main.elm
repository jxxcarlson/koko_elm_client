module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- begin style

import Style exposing (..)
import StyleSheet exposing (..)
import Color
import Element exposing (..)
import Element.Attributes as EA exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition


-- end style

import Window exposing (..)
import Types exposing (..)
import Views.Component exposing (pageSelector)
import Views2.Component as Component
import Views.Home exposing (home)
import Views.Reader exposing (reader)
import Views.Editor exposing (editor)
import Css exposing (asPairs)
import Action.User exposing (..)
import Action.Search exposing (..)
import Action.Document exposing (createDocument, updateDocuments, updateContent, selectDocument, selectNewDocument)
import Data.Document exposing (documents)
import Request.User exposing (loginUserCmd, getTokenCompleted, registerUserCmd)
import Request.Document exposing (getDocumentsWith)
import Request.Api exposing (loginUrl, registerUserUrl)
import Views.Search exposing (documentSearchForm)
import Time exposing (Time, second)
import Views.External exposing (windowData, windowSetup)
import External exposing (render, toJs)
import Request.Document
import Action.UI exposing (displayPage)


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
            if p == EditorPage && model.current_user.token == "" then
                ( { model | page = HomePage, message = "Please sign in if you wish to edit" }, toJs (Views.External.windowData model p) )
            else
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
                ( { model
                    | message = "search: " ++ model.searchState.query
                    , tool = TableOfContents
                    , page = displayPage model
                  }
                , Cmd.batch
                    [ getDocumentsWith model.searchState model.current_user.token
                    , External.render model.current_document.rendered_content
                    ]
                )
            else
                ( model, Cmd.none )

        DoRender key ->
            if key == 27 then
                -- 27: ESCAPE
                ( { model | info = "ESCAPE pressed, rendering ..." }, External.render model.current_document.rendered_content )
            else
                ( model, Cmd.none )

        GetDocuments (Ok serverReply) ->
            case (Data.Document.documents serverReply) of
                Ok documentsRecord ->
                    updateDocuments model documentsRecord

                Err error ->
                    ( { model | info = (toString error) }
                    , Cmd.none
                    )

        GetDocuments (Err _) ->
            ( { model | info = "Error on GET: " ++ (toString Err) }, Cmd.none )

        GetUserDocuments (Ok documentsRecord) ->
            ( { model | documents = documentsRecord.documents }, External.render model.current_document.rendered_content )

        GetUserDocuments (Err error) ->
            ( { model | info = (toString error) }, Cmd.none )

        PutDocument (Ok serverReply) ->
            case (serverReply) of
                () ->
                    ( model, Cmd.none )

        PutDocument (Err _) ->
            ( { model | info = "Error on PUT: " ++ (toString Err) }, Cmd.none )

        NewDocument ->
            let
                newDocument =
                    Document 0 0 "New Document" "New Content" "New Content"
            in
                createDocument model newDocument

        --( model , Request.Document.createDocument newDocument model.current_user.token )
        CreateDocument (Ok documentRecord) ->
            selectNewDocument model documentRecord.document

        CreateDocument (Err errorMessage) ->
            ( { model | info = (toString errorMessage) }, Cmd.none )

        Title title ->
            let
                doc =
                    model.current_document

                new_document =
                    { doc | title = title }
            in
                ( { model | current_document = new_document, message = "Title = " ++ new_document.title }, Cmd.none )

        SelectDocument document ->
            selectDocument model document

        -- ( { model | current_document = document, message = "SelectDocument" }, render document.rendered_content )
        InputContent content ->
            updateContent model content

        Refresh ->
            ( { model | message = "Refresh, rendering" }, External.render model.current_document.rendered_content )

        UseSearchDomain searchDomain ->
            updateSearchDomain model searchDomain

        Tick time ->
            if model.page == EditorPage then
                ( { model | message = "Tick, rendering" }, External.render model.current_document.rendered_content )
                -- ( model, Cmd.none )
            else
                ( model, Cmd.none )

        SendToJS str ->
            ( model, toJs str )

        SetupPages ->
            ( model, toJs (Views.External.windowData model model.page) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (60 * Time.second) Tick
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
    Element.root StyleSheet.stylesheet <|
        column None
            []
            [ Component.navigation model
            , (Component.footer model)
            ]



{-
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

-}
{-
   Element.root StyleSheet.stylesheet <|
       column None
           []
           [ navigation
           , el None [ center, EA.width (px 800) ] <|
               column Main
                   [ spacing 50, paddingTop 50, paddingBottom 50 ]
                   (List.concat
                       [ viewTextLayout
                       , viewRowLayouts
                       , viewGridLayout
                       , viewNamedGridLayout
                       ]
                   )
           ]

-}
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

        content =
            "Welcome"

        rendered_content =
            "Welcome"

        doc =
            Document 0 0 title content rendered_content

        searchState =
            SearchState "" Public

        ws =
            windowSetup 150 50 HomePage False False
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
        , Cmd.batch [ toJs ws, External.render doc.rendered_content ]
        )



--  {"width":1218,"height":686,"page":"HomePage","online":true,"signed_in":false}
