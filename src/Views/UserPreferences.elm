module Views.UserPreferences exposing (userPreferences)

import Types exposing (Model)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick, onInput)
import Element.Keyed as Keyed
import StyleSheet exposing (..)
import Types exposing (..)
import Views.Basic as Basic


userPreferences : Model -> List (Element Styles variation Msg)
userPreferences model =
    [ namedGrid Container
        { columns = [ px 300, fill 1, fill 0.2 ]
        , rows =
            [ px 1 => [ spanAll "separator" ]
            , px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sideBarHeader" ]
            , fill 1 => [ span 1 "BlurbPanel", span 1 "content", span 1 "sidebar" ]
            ]
        }
        []
        [ named "BlurbPanel"
            (column PaleBlue
                [ spacing 10, padding 20, height (px 400) ]
                [ Basic.label "Blurb" Blue []
                , blurbPanel model
                , submitBlurbButton model
                ]
            )
        , named "TOCHeader" (row TOC [] [ (Basic.label "User Preferences" Blue [ width (px 300), height (px 40) ]) ])
        ]
    ]


blurbPanel : Model -> Element Styles variation Msg
blurbPanel model =
    (Keyed.row PaleBlue
        [ height (px 200) ]
        [ ( (toString model.counter)
          , (textArea Mono
                [ width (percent 100)
                , height (px 150)
                , yScrollbar
                , padding 20
                , onInput UpdateTextInputBuffer

                -- , Utility.onKeyUp DoRender
                ]
                (model.current_user.blurb)
            )
          )
        ]
    )


submitBlurbButton : Model -> Element Styles variation Msg
submitBlurbButton model =
    Basic.button
        ("Update")
        Button
        [ onClick UpdateCurrentUser
        , width (px 120)
        , height (px 30)
        ]
