module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


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
    div
        [ class "todomvc-wrapper" ]
        [ section
              [ class "todoapp" ]
              [ viewInput "" ]
        ]


viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            ]
            []
        ]
