module Route exposing
    ( Route(..)
    , matchHome
    , matchSearch
    , matchStats
    , matchTutorial
    , toRoute
    , toUrl
    )

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, top)
import Url.Parser.Query as Query


type Route
    = Home
    | Search String
    | Stats
    | Tutorial


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map (Search << Maybe.withDefault "") (s "search" <?> Query.string "q")
        , Parser.map Stats (s "stats")
        , Parser.map Tutorial (s "tutorial")
        ]


toRoute : Url -> Route
toRoute url =
    case Parser.parse parser url of
        Just r ->
            r

        Nothing ->
            Home


toUrl : Route -> String
toUrl r =
    case r of
        Home ->
            "/"

        Search q ->
            if String.isEmpty q then
                "/search"

            else
                "/search?q=" ++ Url.percentEncode q

        Stats ->
            "/stats"

        Tutorial ->
            "/tutorial"


matchHome : Route -> Maybe ()
matchHome r =
    case r of
        Home ->
            Just ()

        _ ->
            Nothing


matchSearch : Route -> Maybe String
matchSearch r =
    case r of
        Search q ->
            Just q

        _ ->
            Nothing


matchStats : Route -> Maybe ()
matchStats r =
    case r of
        Stats ->
            Just ()

        _ ->
            Nothing


matchTutorial : Route -> Maybe ()
matchTutorial r =
    case r of
        Tutorial ->
            Just ()

        _ ->
            Nothing
