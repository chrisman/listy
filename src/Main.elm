port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Json
import Html.Lazy exposing (lazy, lazy2)



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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        Add ->
          ({ model
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


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ header []
            [ h1 [] [ text "Listy list" ]
            , lazy viewInput model.field
            ]
        , lazy viewEntries model.entries
        ]



-- Custom Event Listener
-- https://package.elm-lang.org/packages/elm/html/latest/Html-Events#on
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
        , label [ for (String.fromInt todo.id) ] [ text todo.description ]
        , button [ class "deleteTodo", onClick (Delete todo.id) ] [ text "🚫" ]
        ]


viewEntries : List Entry -> Html Msg
viewEntries entries =
    section []
        [ Keyed.ul [] <|
            List.map viewKeyedEntry entries
        ]
