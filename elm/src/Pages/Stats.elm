module Pages.Stats exposing (page)

import Api
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Http
import Shared
import Spa.Page as Page
import Ui
import View


type alias Model =
    { loading : Bool
    , error : Maybe String
    , stats : Maybe Api.Stats
    }


type Msg
    = GotStats (Result Http.Error (Api.ApiResponse Api.Stats))


init : () -> ( Model, Effect Shared.Msg Msg )
init _ =
    ( { loading = True, error = Nothing, stats = Nothing }
    , Effect.fromCmd (Api.getStats "https://localhost:3000" GotStats)
    )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg model =
    case msg of
        GotStats (Ok (Api.Ok s)) ->
            ( { model | loading = False, stats = Just s }, Effect.none )

        GotStats (Ok (Api.Err e)) ->
            ( { model | loading = False, error = Just e.message }, Effect.none )

        GotStats (Err httpErr) ->
            ( { model | loading = False, error = Just (Debug.toString httpErr) }, Effect.none )


view : Model -> View.View Msg
view model =
    { title = "Moongle — Stats"
    , body =
        column [ Ui.container, width fill, spacing 18, padding 20 ] <|
            if model.loading then
                [ el [] (text "Loading stats…") ]

            else
                case ( model.error, model.stats ) of
                    ( Just e, _ ) ->
                        [ Ui.card [] (text ("Error: " ++ e)) ]

                    ( _, Just s ) ->
                        [ wrappedRow [ width fill, spacingXY 16 16 ]
                            [ statCard "Packages" s.packages
                            , statCard "Modules" s.modules
                            , statCard "Functions" s.functions
                            , Ui.card [] (el [ Font.size 12 ] (text ("Last indexed: " ++ s.lastIndexed)))
                            ]
                        ]

                    _ ->
                        []
    }


statCard : String -> Int -> Element msg
statCard label n =
    Ui.card [ width (px 260) ] <|
        column [ spacing 8 ]
            [ el [ Font.size 13, Font.color (rgba 255 255 255 0.8) ] (text label)
            , el [ Font.size 28, Font.bold ] (text (String.fromInt n))
            ]


page _ =
    Page.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
