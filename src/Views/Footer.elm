module Views.Footer exposing (footer)

import Configuration
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import List.Extra
import Request.Api as Api
import String.Extra
import StyleSheet exposing (..)
import Types exposing (..)


warningStyle : String -> Styles
warningStyle warning =
    if warning == "" then
        FooterNote
    else if String.contains "OK" warning then
        OKFooterNote
    else
        WarningFooterNote


messageWarningStyle : String -> Styles
messageWarningStyle message =
    if String.contains "!" message then
        WarningFooterNote
    else
        FooterNote


footer : Model -> Element Styles variation msg
footer model =
    case model.device of
        Phone ->
            phoneFooter model

        _ ->
            standardFooter model


standardFooter : Model -> Element Styles variation msg
standardFooter model =
    row Footer
        [ justify, paddingXY 30 4, alignBottom, width (percent 100) ]
        [ messageBox model
        , documentId model
        , shareUrl model
        , warningMessage model
        , onlineStatusIndicator model
        ]


phoneFooter : Model -> Element Styles variation msg
phoneFooter model =
    row Footer
        [ justify, paddingXY 30 4, alignBottom, width (percent 100) ]
        [ publicLink model
        , smallOnlineStatusIndicator model
        ]


publicLink : Model -> Element Styles variation msg
publicLink model =
    let
        linkInfo =
            ". Please follow this link http://www.knode.io/#@public/" ++ toString model.current_document.id

        body =
            "&body=You might be interesed in the article " ++ model.current_document.title ++ linkInfo

        subject =
            "?subject=" ++ model.current_document.title

        linkUrl =
            "mailto:hey@mail" ++ subject ++ body

        linkText =
            if model.current_document.attributes.public == True then
                link linkUrl <| el FooterNote [] <| text ("Share " ++ String.Extra.quote model.current_document.title)
            else
                link "mailto:hey@mail.me" <| el FooterNote [] <| text ""
    in
    el FooterNote [ verticalCenter ] linkText



--  [ href <| "mailto:%22" ++ user.first ++ " " ++ user.last ++ "%22%3c" ++ email ++ "%3e" ] [ text email ]


messageBox : Model -> Element Styles variation msg
messageBox model =
    el (messageWarningStyle model.message) [ alignBottom, padding 8, width (px 200) ] (text model.message)


shareUrl : Model -> Element Styles variation msg
shareUrl model =
    let
        urlText =
            if model.current_document.attributes.public then
                Configuration.client ++ "/#@public/" ++ toString model.current_document.id
            else
                ""
    in
    el FooterNote [ alignBottom, padding 8 ] (text urlText)


documentId : Model -> Element Styles variation msg
documentId model =
    el FooterNote [ alignBottom, padding 8 ] (text ("ID " ++ toString model.current_document.id))



-- if model.appState.signedIn then
--     (el (messageWarningStyle model.message) [ alignBottom, padding 8 ] (text model.message))
-- else
--     (el Zero [ alignBottom, padding 8 ] (text ""))


warningMessage : Model -> Element Styles variation msg
warningMessage model =
    if model.appState.signedIn then
        el (warningStyle model.warning) [ alignBottom, padding 8 ] (text model.warning)
    else
        el Zero [ alignBottom, padding 8 ] (text "")


hostString : String
hostString =
    Api.host |> String.split "//" |> List.Extra.last |> Maybe.withDefault ""


onlineStatusIndicator : Model -> Element Styles variation msg
onlineStatusIndicator model =
    el (onlineStatusStyle model) [ alignBottom, padding 8 ] (text (onlineStatus model ++ " at " ++ hostString))



-- smallOnlineStatusIndicator : Model -> Element Styles variation msg


smallOnlineStatusIndicator model =
    el (onlineStatusStyle model) [ alignBottom, padding 8 ] (text (onlineStatus model))


onlineStatus : Model -> String
onlineStatus model =
    if model.appState.online then
        "Online"
    else
        "Offline"


onlineStatusStyle : Model -> Styles
onlineStatusStyle model =
    if model.appState.online then
        StatusSuccess
    else
        StatusFailure
