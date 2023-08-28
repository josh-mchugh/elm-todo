module Main exposing (main)

import Browser
import Html exposing (..)


main : Program (Model) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { }


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "Hello World" ]
