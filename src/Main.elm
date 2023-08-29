port module Main exposing (main)

{-| TodoMVC implemented in Elm
-}

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Json
import Task



-- MAIN


{-| Elm application entry function
-}
main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }



-- PORT


{-| port to set storage to localState within the browser. Passes the model.
-}
port setStorage : Model -> Cmd msg


{-| Update browser local storage with every update. This function adds
a function call to the `setStorage` port with every call to the update
function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



-- MODEL


type alias Model =
    { field : String
    , uid : Int
    , entries : List Entry
    , visibility : String
    }


type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


emptyModel : Model
emptyModel =
    { field = ""
    , uid = 0
    , entries = []
    , visibility = "All"
    }


newEntry : String -> Int -> Entry
newEntry desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }


{-| Initial application model function
-}
init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | UpdateField String
    | Add
    | Delete Int
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | Check Int Bool
    | DeleteComplete
    | CheckAll Bool
    | ChangeVisibility String


{-| Application update function
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        Add ->
            ( { model
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries

                    else
                        model.entries ++ [ newEntry model.field model.uid ]
              }
            , Cmd.none
            )

        Delete id ->
            ( { model | entries = List.filter (\t -> t.id /= id) model.entries }
            , Cmd.none
            )

        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | editing = isEditing }

                    else
                        t

                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Task.attempt (\_ -> NoOp) focus
            )

        UpdateEntry id task ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = task }

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        Check id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted }

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | entries = List.filter (not << .completed) model.entries }
            , Cmd.none
            )

        CheckAll isCompleted ->
            let
                updateEntry t =
                    { t | completed = isCompleted }
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )



-- VIEW


{-| Main wrapper which contains the entire ToDo application
-}
view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper" ]
        [ section
            [ class "todoapp" ]
            [ viewInput model.field
            , viewEntries model.visibility model.entries
            , viewControls model.visibility model.entries
            ]
        , viewFooter
        ]


{-| Application header with todo header and task input
-}
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
            , onEnter Add
            ]
            []
        ]


{-| function to handle the "on" input event for "keydown" when the
"enter" key is pressed.
-}
onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


{-| Section wrapper for the main part of the app with the list
of tasks.
-}
viewEntries : String -> List Entry -> Html Msg
viewEntries visibility entries =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True

        allCompleted =
            List.all .completed entries
    in
    section [ class "main" ]
        [ input
            [ class "toggle-all"
            , type_ "checkbox"
            , name "toggle"
            , checked allCompleted
            , onClick (CheckAll (not allCompleted))
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list" ] <|
            List.map viewKeyedEntry (List.filter isVisible entries)
        ]


{-| Keyed entry view function, this function returns the entry view
in a tuple with the task id
-}
viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry todo =
    ( String.fromInt todo.id, viewEntry todo )


{-| Task Entry view for the task items
-}
viewEntry : Entry -> Html Msg
viewEntry todo =
    li
        [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick (Check todo.id (not todo.completed))
                ]
                []
            , label [ onDoubleClick (EditingEntry todo.id True) ]
                [ text todo.description ]
            , button
                [ class "destroy"
                , onClick (Delete todo.id)
                ]
                []
            ]
        , input
            [ class "edit"
            , value todo.description
            , name "title"
            , id ("todo-" ++ String.fromInt todo.id)
            , onInput (UpdateEntry todo.id)
            , onBlur (EditingEntry todo.id False)
            , onEnter (EditingEntry todo.id False)
            ]
            []
        ]


{-| Wrapper view for the application controls
-}
viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ viewControlsCount entriesLeft
        , viewControlsFilter visibility
        , viewControlsClear entriesCompleted
        ]


{-| View for tasks count for controls view
-}
viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"

            else
                " items"
    in
    span
        [ class "todo-count" ]
        [ strong [] [ text (String.fromInt entriesLeft) ]
        , text (item_ ++ " left")
        ]


{-| View for clear completed tasks button for controls view
-}
viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")") ]


{-| View for the filter tasks action items for the controls view
-}
viewControlsFilter : String -> Html Msg
viewControlsFilter visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]


{-| View for filter items within the filter tasks view
-}
visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text visibility ]
        ]


{-| Application footer with author infomation and project references
-}
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
