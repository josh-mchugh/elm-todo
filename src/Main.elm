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


update : Msg -> Model -> ( Model, Cmd Msg )
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
        , viewFooter
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


viewFooter : Html msg
viewFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/josh-mchugh" ] [ text "Josh McHugh" ]
            ]
        , p []
            [ text "Original written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "https://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
