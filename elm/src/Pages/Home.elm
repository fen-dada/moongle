module Pages.Home exposing (..)

import Effect exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Route
import Shared
import Spa
import Spa.Page as Page
import Ui
import View


type alias Model =
    { q : String }


type Msg
    = SetQ String


type alias Flags =
    ()


init : Flags -> ( Model, Effect Shared.Msg Msg )
init _ =
    ( { q = "" }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg model =
    case msg of
        SetQ s ->
            ( { model | q = s }, Effect.none )


view : Model -> View.View Msg
view model =
    { title = "Moongle — Search"
    , body =
        column [ width fill, height fill, centerX, centerY ]
            [ el [ height (px 120) ] Element.none
            , Ui.card [ Ui.container, padding 24 ] <|
                column [ spacing 18, width (fill |> maximum 800) ]
                    [ el [ Font.size 28, Font.color (rgb255 255 255 255), Font.bold ] (text "Find MoonBit APIs fast")
                    , row [ spacing 12, width fill ]
                        [ el [ width fill ] <|
                            Ui.field { placeholder = "Try: [K, V](K, V) -> Unit", value = model.q, onChange = SetQ }
                        , Ui.primaryButton
                            { label = "Search"
                            , url = Route.toUrl (Route.Search model.q)
                            }
                        ]
                    , el [ Font.color (rgba 255 255 255 0.75), Font.size 14 ]
                        (text "Type-like search • Prefix match on module paths • Filters coming soon")
                    ]
            , el [ height (px 140) ] Element.none
            ]
    }



-- page : Shared.Model -> Page.Page View () Shared.Model Model Msg
-- page _ =
--     Page.sandbox
--         { init = init
--         , update = update
--         , view = view
--         }
