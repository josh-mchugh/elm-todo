module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { field : String }


emptyModel : Model
emptyModel =
    { field = "" }


init : () -> ( Model, Cmd Msg )
init () =
    ( emptyModel, Cmd.none )


type Msg
    = NoOp
    | UpdateField String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper" ]
        [ section
              [ class "todoapp" ]
              [ viewInput model.field ]
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
            , onInput UpdateField
            ]
            []
        ]
