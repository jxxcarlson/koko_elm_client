module Views.Reader exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import FontAwesome
import Json.Encode
import StyleSheet exposing (..)
import Types exposing (..)
import Views.Basic as Basic
import Views.Common as Common


reader : Model -> List (Element Styles variation Msg)
reader model =
    case model.device of
        Phone ->
            phoneReader model

        Tablet ->
            tabletReader model

        _ ->
            standardReader model


standardReader : Model -> List (Element Styles variation Msg)
standardReader model =
    [ namedGrid Container
        { columns = [ fill 2, fill 5, fill 3 ]
        , rows =
            [ px 1 => [ spanAll "separator" ]
            , px 40 => [ span 1 "TOCHeader", span 1 "contentHeader", span 1 "sidebarHeader" ]
            , fill 1 => [ span 1 "TOC", span 1 "content", span 1 "sidebar" ]
            ]
        }
        []
        [ named "separator" (hairline Hairline)
        , named "TOCHeader" (toolSelectorPanel model)
        , named "contentHeader" (contentHeader model)
        , named "content" (Common.renderedContent model)
        , named "TOC" (Common.tool model)
        , named "sidebarHeader" (rhSidebarHeader model)
        , named "sidebar" (specialContent model)
        ]
    ]


tabletReader : Model -> List (Element Styles variation Msg)
tabletReader model =
    [ namedGrid Container
        { columns = [ fill 2, fill 5 ]
        , rows =
            [ px 1 => [ spanAll "separator" ]
            , px 40 => [ span 1 "TOCHeader", span 1 "contentHeader" ]
            , fill 1 => [ span 1 "TOC", span 1 "content" ]
            ]
        }
        []
        [ named "separator" (hairline Hairline)
        , named "TOCHeader" (toolSelectorPanel model)
        , named "contentHeader" (contentHeader model)
        , named "content" (Common.renderedContent model)
        , named "TOC" (Common.tool model)
        ]
    ]


phoneReader : Model -> List (Element Styles variation Msg)
phoneReader model =
    [ namedGrid Container
        { columns = [ fill 1 ]
        , rows =
            [ px 1 => [ spanAll "separator" ]
            , px 40 => [ span 1 "contentHeader" ]
            , fill 1 => [ span 1 "content" ]
            ]
        }
        []
        [ named "separator" (hairline Hairline)
        , named "contentHeader" (contentHeader model)
        , named "content" (Common.renderedContentForPhone model)
        ]
    ]


rhSidebarHeader : Model -> Element Styles variation Msg
rhSidebarHeader model =
    el RHSidebarHeader
        [ paddingXY 20 8, verticalCenter, onClick (DocMsg EditSpecialDocument) ]
        (text model.specialDocument.title)


specialContent : Model -> Element Styles variation msg
specialContent model =
    let
        h =
            toFloat model.window.height - 120
    in
    el RHSidebar
        [ yScrollbar
        , id "rendered_text2"
        , paddingXY 20 20
        , height (px h)
        , property "innerHTML"
            (Json.Encode.string model.specialDocument.rendered_content)
        ]
        (text "")


rhSidebar : Model -> Element Styles variation msg
rhSidebar model =
    let
        h =
            toFloat model.window.height - 120
    in
    el RHSidebar [ padding 0, width (percent 100), height (px h) ] (text "")


contentHeader : Model -> Element Styles variation Msg
contentHeader model =
    row NavBar
        [ justify ]
        [ el TitleStyle [ paddingXY 10 8 ] (text model.current_document.title)
        , authorLink model
        ]


authorLink : Model -> Element Styles variation Msg
authorLink model =
    let
        doc =
            model.current_document

        author_name =
            doc.author_name

        query =
            "authorname=" ++ author_name ++ "&key=home"
    in
    el AuthorStyle
        [ onClick (PageMsg (GetHomePageForUserHomePages query author_name))
        , verticalCenter
        , paddingXY 16 13
        ]
        (text author_name)


toolSelectorPanel : Model -> Element Styles variation Msg
toolSelectorPanel model =
    row Panel
        [ paddingXY 20 6, justify ]
        [ Common.printButton model.current_document
        , Common.recallLastSearchButton model
        , Common.toggleListView model
        ]
