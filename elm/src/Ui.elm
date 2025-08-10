module Ui exposing
    ( bgDark
    , bgLight
    , card
    , container
    , field
    , logo
    , navExternal
    , navLink
    , pill
    , primaryButton
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes


bgDark : Element.Color
bgDark =
    rgb255 12 14 20


bgLight : Element.Color
bgLight =
    rgb255 24 27 38


container : Attribute msg
container =
    width (fill |> maximum 1280)


logo : Element msg
logo =
    row [ spacing 10, centerY ]
        [ el [ Font.bold, Font.size 18 ] (text "🌙 Moongle")
        ]


navLink : String -> String -> Element msg
navLink label url =
    link
        [ mouseOver [ Font.color (rgb255 255 255 255) ]
        , Font.color (rgba 255 255 255 0.86)
        , htmlAttribute (Html.Attributes.style "transition" "all 160ms ease")
        , paddingXY 10 6
        , Border.rounded 10
        , mouseOver [ Background.color (rgba 255 255 255 0.12) ]
        ]
        { url = url
        , label = text label
        }


navExternal : String -> String -> Element msg
navExternal label url =
    newTabLink
        [ Font.color (rgba 255 255 255 0.86)
        , mouseOver [ Background.color (rgba 255 255 255 0.12) ]
        , paddingXY 10 6
        , Border.rounded 10
        ]
        { url = url, label = text label }


card : List (Attribute msg) -> Element msg -> Element msg
card attrs content =
    el
        ([ Background.color (rgba 255 255 255 0.06)
         , Border.rounded 18
         , Border.width 1
         , Border.color (rgba 255 255 255 0.18)
         , Border.shadow { offset = ( 0, 14 ), size = 0, blur = 28, color = rgba 0 0 0 0.25 }
         , padding 18
         , htmlAttribute (Html.Attributes.style "transition" "transform 160ms ease, box-shadow 160ms ease, background 160ms ease")
         , mouseOver [ Background.color (rgba 255 255 255 0.09) ]
         ]
            ++ attrs
        )
        content


pill : String -> Element msg
pill label =
    el
        [ Border.rounded 999
        , paddingXY 10 4
        , Background.color (rgba 255 255 255 0.14)
        , Font.size 12
        ]
        (text label)


field : { placeholder : String, value : String, onChange : String -> msg } -> Element msg
field cfg =
    Input.text
        [ width fill
        , height (px 54)
        , paddingXY 18 8
        , Background.color (rgba 255 255 255 0.86)
        , Border.rounded 14
        , Border.width 2
        , Border.color (rgba 255 255 255 0)
        , mouseOver [ Border.color (rgba 255 255 255 0.6) ]
        , focused [ Border.color (rgba 255 255 255 1) ]
        ]
        { onChange = cfg.onChange
        , text = cfg.value
        , placeholder = Just (Input.placeholder [] (text cfg.placeholder))
        , label = Input.labelHidden "Search"
        }


primaryButton : { label : String, url : String } -> Element msg
primaryButton cfg =
    link
        [ paddingXY 18 12
        , Border.rounded 12
        , Background.gradient
            { angle = degrees 15
            , steps =
                [ rgb255 149 118 255
                , rgb255 0 200 255
                ]
            }
        , Font.bold
        , Font.color (rgb255 12 14 20)
        , mouseDown [ Background.color (rgb255 210 210 210) ]
        , mouseOver [ Background.color (rgb255 245 245 245) ]
        ]
        { url = cfg.url
        , label = text cfg.label
        }
