module Shared exposing (Model, Msg(..), identity, init, subscriptions, update)

import Browser.Navigation as Nav


type alias Model =
    { key : Nav.Key
    , apiBase : String
    }


type Msg
    = Noop


init : () ->  Nav.Key -> ( Model, Cmd Msg )
init _ key =
    ( { key = key
      , apiBase = "http://localhost:3000"
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


identity : Model -> Maybe ()
identity _ =
    Nothing
