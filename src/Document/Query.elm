module Document.Query exposing (cleanQuery, makeQuery, processorAndRoute)

import Document.QueryParser exposing (parseQuery)
import Http
import Types
    exposing
        ( DocMsg(..)
        , SearchDomain(..)
        , SearchOrder(..)
        , SearchState
        )


cleanQuery : String -> String
cleanQuery query =
    String.split "&" query
        |> List.filter (\item -> not (String.contains "random" item))
        |> String.join "&"


makeQuery : SearchState -> SearchDomain -> Int -> String
makeQuery searchState updatedSearchDomain user_id =
    let
        rawQuery =
            searchState.query

        cmd =
            rawQuery |> String.split "=" |> List.head |> Maybe.withDefault "NoCommand"
    in
    if List.member cmd [ "idlist" ] then
        rawQuery
    else
        makeQueryHelper searchState updatedSearchDomain user_id


makeQueryHelper : SearchState -> SearchDomain -> Int -> String
makeQueryHelper searchState updatedSearchDomain user_id =
    let
        basicQuery =
            -- if searchState.query == "" then
            --     "publicdocs=all"
            -- else
            parseQuery searchState.query

        soq =
            searchOrderQuery searchState.order

        prefix =
            case ( updatedSearchDomain, searchState.query ) of
                ( All, "" ) ->
                    "random=all"

                ( Public, "" ) ->
                    "random=public"

                -- ( Public, _ ) ->
                --     "public=yes"
                ( Private, "" ) ->
                    "random_user=" ++ toString user_id

                ( All, _ ) ->
                    "docs=any"

                ( _, _ ) ->
                    ""

        queryList =
            [ prefix ] ++ [ parseQuery searchState.query, soq ]
    in
    buildQuery queryList


processorAndRoute : SearchDomain -> ( Result Http.Error Types.DocumentsRecord -> DocMsg, String )
processorAndRoute searchDomain =
    case searchDomain of
        Public ->
            ( GetDocuments, "public/documents" )

        Private ->
            ( GetUserDocuments, "documents" )

        All ->
            ( GetUserDocuments, "documents" )


searchOrderQuery : SearchOrder -> String
searchOrderQuery searchOrder =
    case searchOrder of
        Viewed ->
            "sort=viewed"

        Updated ->
            "sort=updated"

        Created ->
            "sort=created"

        Alphabetical ->
            "sort=title"


buildQuery : List String -> String
buildQuery queryParts =
    queryParts
        |> List.filter (\x -> x /= "")
        |> String.join "&"
