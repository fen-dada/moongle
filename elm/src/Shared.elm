module Shared exposing (Model, Msg(..), identity, init, subscriptions, update)

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)


type alias Model =
    { key : Nav.Key
    , apiBase : String
    }


type Msg
    = Noop


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
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
