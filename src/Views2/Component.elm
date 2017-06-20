module Views2.Component exposing (..)

import Style exposing (..)
import StyleSheet exposing (..)
import Color
import Html as Html
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition
import Types exposing (..)
import Html.Events as HE exposing (onClick)
import Json.Decode as Json exposing (int, list, string, float, Decoder)


-- import Utility exposing (onKeyUp)
-- end style --
-- button [ onClick (GoTo HomePage), HA.class (selectedClass HomePage model) ] [ Html.text "Home" ]
{-

   documentSearchForm : Model -> Html Msg
   documentSearchForm model =
       div [ id "SearchForm" ]
           [ input
               [ id "searchInputField"
               , type_ "text"
               , placeholder "Search"
                 --, Html.Attributes.value model.input_text
               , onInput SetSearchTerm
               , Utility.onKeyUp DoSearch
                 -- if keyCode == 13
               ]
               []
           ]


-}
-- inputText Style [] "The Value!"


activeButton : Page -> Model -> Styles
activeButton currentPage model =
    if currentPage == model.appState.page then
        ActiveButton
    else
        Button



-- onKeyUp : (Int -> msg) -> Attribute msg
-- onKeyUp tagger =
--     on "keyup" (Json.map tagger EE.keycode)


navigation : Model -> Element Styles variation Msg
navigation model =
    row NavBar
        [ justify, paddingXY 30 4 ]
        [ el Logo [ alignBottom, padding 8 ] (text "Noteshare")
        , inputText SearchField [ EE.onInput SetSearchTerm, placeholder "Search" ] ("")
        , menu model
        , pageSelector model
        , loginButton model
        ]


loginButton model =
    el Button
        [ EA.width (px 85)
        , EA.center
        , EE.onClick AuthenticationAction
        ]
        (paragraph None
            [ EA.height (px 30)
            , padding 8
            ]
            [ EL.text (authenticationButtonText model) ]
        )


authenticationButtonText : Model -> String
authenticationButtonText model =
    if model.appState.signedIn then
        "Sign out"
    else
        "Sign in"


menu model =
    el Button [ EA.width (px 85), EA.center, EE.onClick ToggleMenu ] (paragraph None [ EA.height (px 30), padding 8 ] [ EL.text "Tools" ])
        |> below
            [ when model.appState.menuDropped <|
                column None
                    [ padding 8, spacing 8 ]
                    [ link "#1" (text "Action 1")
                    , link "#2" (text "Action 2")
                    , link "#3" (text "Action 3")
                    ]
            ]



-- div [ id "footer" ]
--     [ span [ id "message" ] [ text model.message ]
--     , span [ id "info" ] [ text model.info ]
--     ]


pageSelector : Model -> Element Styles variation Msg
pageSelector model =
    row None
        [ spacing 20 ]
        [ el (activeButton HomePage model) [ EE.onClick (GoTo HomePage), alignBottom, height (px 30), padding 8 ] (text "Home")
        , el (activeButton ReaderPage model) [ EE.onClick (GoTo ReaderPage), alignBottom, height (px 30), padding 8 ] (text "Reader")
        , el (activeButton EditorPage model) [ EE.onClick (GoTo EditorPage), alignBottom, height (px 30), padding 8 ] (text "Editor")
        ]


footer : Model -> Element Styles variation msg
footer model =
    (row Footer
        [ justify, paddingXY 30 4 ]
        [ el None [ alignBottom, padding 8 ] (text model.message)
        , el None [ alignBottom, padding 8 ] (text model.info)
        ]
    )
