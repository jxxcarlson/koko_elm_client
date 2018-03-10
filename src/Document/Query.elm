module Document.Query exposing (cleanQuery, fixQueryIfEmpty, makeQuery, processorAndRoute)

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


fixQueryIfEmpty : String -> SearchDomain -> Int -> String
fixQueryIfEmpty query searchDomain user_id =
    if query == "" then
        case searchDomain of
            Public ->
                "random=public"

            Private ->
                "random_user=" ++ toString user_id

            Shared ->
                "random_user=" ++ toString user_id

            All ->
                "random=all"
    else
        query


makeQuery : SearchState -> SearchDomain -> Int -> String
makeQuery searchState updatedSearchDomain user_id =
    let

        rawQuery =
            searchState.query

        cmd =
            rawQuery |> String.split "=" |> List.head |> Maybe.withDefault "NoCommand"

        _ = Debug.log("rawQuery") (updatedSearchDomain, rawQuery, cmd)  
         
    in
    if List.member cmd [ "idlist" ] then
        rawQuery
    else
        makeQueryHelper searchState updatedSearchDomain user_id


makeQueryHelper : SearchState -> SearchDomain -> Int -> String
makeQueryHelper searchState updatedSearchDomain user_id =
    let 
        suffix = case updatedSearchDomain of 
            Private -> "shared=yes"
            Shared -> "shared_only=yes"
            Public -> ""
            All -> ""

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
            [ prefix ] ++ [ parseQuery searchState.query, soq, suffix ]
    in
    buildQuery queryList


processorAndRoute : SearchDomain -> ( Result Http.Error Types.DocumentsRecord -> DocMsg, String )
processorAndRoute searchDomain =
    case searchDomain of
        Public ->
            ( GetDocuments, "public/documents" )

        Private ->
            ( GetUserDocuments, "documents" )

        Shared ->

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
