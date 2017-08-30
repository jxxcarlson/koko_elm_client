module Views.Footer exposing (footer)

import StyleSheet exposing (..)
import Element as EL exposing (..)
import Element.Attributes as EA exposing (..)
import Types exposing (..)
import StyleSheet exposing (..)
import Request.Api as Api
import List.Extra

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
    (row Footer
        [ justify, paddingXY 30 4, alignBottom, width (percent 100) ]
          [
            (messageBox model)
            , (publicLink model)
            , (warningMessage model)
            , (onlineStatusIndicator model)
         ]
    )

publicLink : Model -> Element Styles variation msg
publicLink model =
  let
    linkText = if model.current_document.attributes.public == True then
         "Share as http://www.knode.io/##public/" ++ (toString model.current_document.id)
       else
          ""
  in
    el FooterNote [verticalCenter] (text linkText )

messageBox : Model -> Element Styles variation msg
messageBox model =
  if model.appState.signedIn then
    (el (messageWarningStyle model.message) [ alignBottom, padding 8 ] (text model.message))
  else
    (el Zero [ alignBottom, padding 8 ] (text ""))

warningMessage : Model -> Element Styles variation msg
warningMessage model =
  if model.appState.signedIn then
    (el (warningStyle model.warning) [ alignBottom, padding 8 ] (text model.warning))
  else
    (el Zero [ alignBottom, padding 8 ] (text ""))

hostString : String
hostString =
  Api.host |> String.split("//") |> List.Extra.last |> Maybe.withDefault ""

onlineStatusIndicator : Model -> Element Styles variation msg
onlineStatusIndicator model =
  el (onlineStatusStyle model) [ alignBottom, padding 8 ] (text ((onlineStatus model) ++ " at " ++ hostString ))

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
