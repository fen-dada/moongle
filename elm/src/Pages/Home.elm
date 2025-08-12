module Pages.Home exposing (page)

import Effect exposing (..)
import Element exposing (..)
import Element.Font as Font
import Route
import Shared
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


update : Msg -> ( Model, Effect Shared.Msg Msg ) -> ( Model, Effect Shared.Msg Msg )
update msg ( model, _ ) =
    case msg of
        SetQ s ->
            ( { model | q = s }, Effect.none )


view : ( Model, Effect Shared.Msg Msg ) -> View.View Msg
view ( model, _ ) =
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


page : a -> Page.Page Flags sharedMsg (View.View Msg) ( Model, Effect Shared.Msg Msg ) Msg
page _ =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }
