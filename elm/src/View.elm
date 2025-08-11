module View exposing (View, defaultView, map, toDocument)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import Route
import Shared
import Ui


type alias View msg =
    { title : String
    , body : Element msg
    }


map : (a -> b) -> View a -> View b
map f v =
    { title = v.title
    , body = Element.map f v.body
    }


defaultView : View msg
defaultView =
    { title = "Moongle"
    , body = none
    }


toDocument : Shared.Model -> View msg -> Browser.Document msg
toDocument shared view =
    { title = view.title
    , body =
        [ layout [] <|
            column
                [ width fill
                , height fill
                , Background.gradient { angle = degrees 180, steps = [ Ui.bgDark, Ui.bgLight ] }
                ]
                [ navbar shared
                , el [ width fill, height fill ] view.body
                ]
        ]
    }


navbar : Shared.Model -> Element msg
navbar _ =
    el
        [ width fill
        , paddingXY 20 14
        , Background.color (rgba 255 255 255 0.55)
        , Border.rounded 18
        , Border.width 1
        , Border.color (rgba 255 255 255 0.8)
        , Border.shadow { offset = ( 0, 8 ), size = 0, blur = 24, color = rgba 0 0 0 0.12 }
        , inFront glossy
        , Ui.container
        , htmlAttribute (Html.Attributes.style "backdrop-filter" "saturate(180%) blur(14px)")
        ]
    <|
        row [ width fill, centerY, spacing 10 ]
            [ Ui.logo
            , el [ width fill ] none
            , row [ spacing 22, centerY ]
                [ Ui.navLink "Tutorial" (Route.toUrl Route.Tutorial)
                , Ui.navLink "Stats" (Route.toUrl Route.Stats)
                , Ui.navExternal "GitHub" "https://github.com/HCHogan/moongle"
                ]
            ]


glossy : Element msg
glossy =
    el
        [ width fill
        , height (px 1)
        , alignTop
        , Background.gradient { angle = degrees 180, steps = [ rgba 255 255 255 0.8, rgba 255 255 255 0 ] }
        ]
        none
