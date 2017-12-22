module User.Search exposing (form)

import Color
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Element.Events as EE exposing (..)
import FontAwesome
import StyleSheet exposing (..)
import Types exposing (Model, Msg(SearchMsg), SearchMsg(..))
import Utility


form : Model -> Element Styles variation Msg
form model =
    row NavBar
        [ spacing 10, verticalCenter ]
        [ row Zero
            [ spacing -30 ]
            [ inputText SearchField
                [ EE.onInput (SearchMsg << UpdateSearchQueryInputBuffer)
                , Utility.onKeyUp (SearchMsg << SearchForUserHomePages)
                , placeholder "Search for a user (name, keyword)"
                , title "Enter a username or a keyword"
                , height (px 29)
                , width (px 300)
                , paddingXY -20 0
                ]
                model.searchQueryInputBuffer
            , circle 10 ClearButton [ verticalCenter, paddingXY 6.5 9.0, onClick (SearchMsg ClearSearch) ] (text "x")
            ]

        -- , row Zero
        --     [ center, spacing 15, paddingXY 10 0]
        --     [ searchButton model
        --     ]
        ]


searchButton : Model -> Element Styles variation Msg
searchButton model =
    el Zero
        [ EA.width (px 25)
        , title "Search for my documents"
        , EA.alignRight
        , EE.onClick ((SearchMsg << SearchForUserHomePages) 13)
        , EA.height (px 30)
        , paddingXY 0 4
        ]
        searchIcon


searchIcon : Element style variation msg
searchIcon =
    html (FontAwesome.search Color.white 25)
