module Main exposing (main)

import Browser
import Pages.Home as Home
import Pages.Search as Search
import Pages.Stats as Stats
import Pages.Tutorial as Tutorial
import Route
import Shared
import Spa
import View


main =
    Spa.init
        { defaultView = View.defaultView
        , extractIdentity = Shared.identity
        }
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchHome Home.page
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchSearch Search.page
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchStats Stats.page
        |> Spa.addPublicPage ( View.map, View.map ) Route.matchTutorial Tutorial.page
        |> Spa.application View.map
            { toRoute = Route.toRoute
            , init = Shared.init
            , update = Shared.update
            , subscriptions = Shared.subscriptions
            , toDocument = View.toDocument
            , protectPage = \_ -> Route.toUrl Route.Home
            }
        |> Browser.application
