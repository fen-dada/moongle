module Pages.Search exposing (page)

import Api
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Html
import Http
import Route
import Shared
import Spa.Page as Page
import Ui
import View


type alias Flags =
    String


type alias Model =
    { q : String
    , loading : Bool
    , error : Maybe String
    , res : Maybe Api.SearchRes
    }


type Msg
    = SetQ String
    | Submit
    | GotRes (Result Http.Error (Api.ApiResponse Api.SearchRes))
    | OnNewFlags Flags


init : Flags -> ( Model, Effect Shared.Msg Msg )
init fq =
    let
        model =
            { q = fq, loading = False, error = Nothing, res = Nothing }
    in
    if String.isEmpty fq then
        ( model, Effect.none )

    else
        ( { model | loading = True }
        , Effect.fromCmd (Api.postSearch "http://localhost:3000" { q = fq, limit = Nothing, offset = Nothing, lang = Nothing } GotRes)
        )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg model =
    case msg of
        SetQ s ->
            ( { model | q = s }, Effect.none )

        Submit ->
            if String.isEmpty model.q then
                ( model, Effect.none )

            else
                ( { model | loading = True, error = Nothing }
                , Effect.fromCmd (Api.postSearch "http://localhost:3000" { q = model.q, limit = Nothing, offset = Nothing, lang = Nothing } GotRes)
                )

        GotRes (Ok (Api.Ok data)) ->
            ( { model | loading = False, res = Just data, error = Nothing }
            , Effect.none
            )

        GotRes (Ok (Api.Err e)) ->
            ( { model | loading = False, error = Just e.message }
            , Effect.none
            )

        GotRes (Err httpErr) ->
            ( { model | loading = False, error = Just (Debug.toString httpErr) }
            , Effect.none
            )

        OnNewFlags newQ ->
            -- URL changed but we're still on /search — refresh results without full re-init
            update Submit { model | q = newQ }


view : Model -> View.View Msg
view model =
    { title = "Moongle — Search"
    , body =
        column [ Ui.container, width fill, spacing 18, paddingEach { top = 28, bottom = 40, left = 20, right = 20 } ]
            [ -- top search bar (sticky-ish look)
              Ui.card [ padding 16 ] <|
                row [ spacing 12, width fill ]
                    [ el [ width fill ]
                        (Ui.field { placeholder = "Type your query…", value = model.q, onChange = SetQ })
                    , link [ Border.rounded 12 ]
                        { url = Route.toUrl (Route.Search model.q)
                        , label = paragraph [] [ text "Search" ]
                        }
                    ]
            , case ( model.loading, model.error, model.res ) of
                ( True, _, _ ) ->
                    el [] (text "Searching…")

                ( _, Just e, _ ) ->
                    Ui.card [] (text ("Error: " ++ e))

                ( _, _, Just res ) ->
                    column [ spacing 16 ]
                        ([ row [ spacing 10, centerY ]
                            [ el [ Font.size 16, Font.color (rgba 255 255 255 0.85) ] (text (String.fromInt res.hitsTotal ++ " results"))
                            ]
                         ]
                            ++ List.map viewHit res.items
                        )

                _ ->
                    none
            ]
    }


viewHit : Api.SearchHit -> Element msg
viewHit h =
    Ui.card [ width fill ] <|
        column [ spacing 6 ]
            [ wrappedRow [ width fill, spacingXY 8 8 ]
                [ el [ Font.bold ] (text (h.user ++ " / " ++ String.join "/" h.package))
                , Ui.pill h.mod_
                ]
            , el [] (text h.decl)
            , el [ Font.color (rgba 255 255 255 0.7), Font.size 12 ] (text ("score " ++ String.fromInt h.score))
            ]


page _ =
    Page.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
        |> Page.onNewFlags OnNewFlags
