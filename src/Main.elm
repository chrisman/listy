port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Task



-- MAIN


main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { init = init
        , update = updateWithStorage
        , subscriptions = subscriptions
        , view = view
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
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
    { entries : List Entry
    , field : String
    , uid : Int
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
    { entries = []
    , field = ""
    , uid = 0
    , visibility = "All"
    }


newEntry : String -> Int -> Entry
newEntry desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = Add
    | UpdateField String
    | Delete Int
    | Check Int Bool
    | ChangeVisibility String
    | DeleteComplete
    | EditingEntry Int Bool
    | NoOp
    | UpdateEntry Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            ( { model
                | entries = List.filter (\t -> t.id /= id) model.entries
              }
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

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | entries = List.filter (not << .completed) model.entries }
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

        NoOp ->
            ( model, Cmd.none )

        UpdateEntry id desc ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = desc }

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ header []
            [ h1 [] [ text "Listy List" ]
            , lazy viewInput model.field
            ]
        , lazy2 viewControls model.visibility model.entries
        , lazy2 viewEntries model.visibility model.entries
        ]


{-| Custom Event Listener
<https://package.elm-lang.org/packages/elm/html/latest/Html-Events#on>
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


viewInput : String -> Html Msg
viewInput field =
    input
        [ placeholder "add an item"
        , class "todo"
        , value field
        , autofocus True
        , onInput UpdateField
        , onEnter Add
        ]
        []


viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry todo =
    ( String.fromInt todo.id, viewEntry todo )


viewEntry : Entry -> Html Msg
viewEntry todo =
    li []
        [ input
            [ type_ "checkbox"
            , checked todo.completed
            , onClick (Check todo.id (not todo.completed))
            , id (String.fromInt todo.id)
            ]
            []
        , label
            [ classList [ ( "hidden", todo.editing ), ( "complete", todo.completed ) ]
            , class "grow"
            , for (String.fromInt todo.id)
            ]
            [ text todo.description ]
        , input
            [ class "edit"
            , class "grow"
            , value todo.description
            , classList [ ( "hidden", not todo.editing ) ]
            , id ("todo-" ++ String.fromInt todo.id)
            , onInput (UpdateEntry todo.id)
            , onBlur (EditingEntry todo.id False)
            , onEnter (EditingEntry todo.id False)
            ]
            []
        , button [ onClick (EditingEntry todo.id True) ] [ text "✏️" ]
        , button [ onClick (Delete todo.id) ] [ text "🚫" ]
        ]


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
    in
    section []
        [ Keyed.ul [] <|
            List.map viewKeyedEntry (List.filter isVisible entries)
        ]


viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
    div [ class "controls", hidden (List.isEmpty entries) ]
        [ div []
            [ lazy viewControlsCount entriesLeft
            , lazy viewControlsClear entriesCompleted
            ]
        , lazy viewControlsFilters visibility
        ]


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


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , visibilitySwap "#/active" "Active" visibility
        , visibilitySwap "#/completed" "Completed" visibility
        ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text visibility ]
        ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]
