module Api exposing
    ( ApiError
    , ApiResponse(..)
    , SearchHit
    , SearchReq
    , SearchRes
    , Stats
    , decodeApiResponse
    , getStats
    , postSearch
    )

import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type ApiResponse a
    = Ok a
    | Err ApiError


type alias ApiError =
    { code : String
    , message : String
    }


apiError : Decoder ApiError
apiError =
    D.map2 ApiError
        (D.field "code" D.string)
        (D.field "message" D.string)


type alias SearchReq =
    { q : String
    , limit : Maybe Int
    , offset : Maybe Int
    , lang : Maybe String
    }


encodeSearchReq : SearchReq -> E.Value
encodeSearchReq r =
    let
        base =
            ( "q", E.string r.q )
                :: (case r.limit of
                        Just n ->
                            [ ( "limit", E.int n ) ]

                        Nothing ->
                            []
                   )
                ++ (case r.offset of
                        Just n ->
                            [ ( "offset", E.int n ) ]

                        Nothing ->
                            []
                   )
                ++ (case r.lang of
                        Just s ->
                            [ ( "lang", E.string s ) ]

                        Nothing ->
                            []
                   )
    in
    E.object
        [ ( "dat", E.object base ) ]


type alias SearchHit =
    { user : String
    , mod_ : String
    , package : List String
    , decl : String
    , score : Int
    }


searchHit : Decoder SearchHit
searchHit =
    D.map5 SearchHit
        (D.field "user" D.string)
        (D.field "mod" D.string)
        (D.field "package" (D.list D.string))
        (D.field "decl" D.string)
        (D.field "score" D.int)


type alias SearchRes =
    { hitsTotal : Int
    , items : List SearchHit
    }


searchRes : Decoder SearchRes
searchRes =
    D.map2 SearchRes
        (D.field "hitsTotal" D.int)
        (D.field "items" (D.list searchHit))


type alias Stats =
    { packages : Int
    , modules : Int
    , functions : Int
    , lastIndexed : String
    }


stats : Decoder Stats
stats =
    D.map4 Stats
        (D.field "packages" D.int)
        (D.field "modules" D.int)
        (D.field "functions" D.int)
        (D.field "lastIndexed" D.string)


decodeApiResponse : Decoder a -> Decoder (ApiResponse a)
decodeApiResponse inner =
    D.field "status" D.string
        |> D.andThen
            (\status ->
                if status == "ok" then
                    D.field "dat" inner |> D.map Ok

                else
                    D.field "err" apiError |> D.map Err
            )


postSearch : String -> SearchReq -> (Result Http.Error (ApiResponse SearchRes) -> msg) -> Cmd msg
postSearch base req toMsg =
    Http.post
        { url = base ++ "/api/search"
        , body = Http.jsonBody (encodeSearchReq req)
        , expect = Http.expectJson toMsg (decodeApiResponse searchRes)
        }


getStats : String -> (Result Http.Error (ApiResponse Stats) -> msg) -> Cmd msg
getStats base toMsg =
    Http.get
        { url = base ++ "/api/stats"
        , expect = Http.expectJson toMsg (decodeApiResponse stats)
        }
