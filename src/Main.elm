port module Main exposing (Model, Msg(..), init, main, toJs, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onCheck, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------

type alias Todo =
    { text : String
    , done : Bool
    }

type alias Model =
    List Todo

init : Int -> ( Model, Cmd Msg )
init flags =
    ( [] , Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Add
    | Remove Int
    | Change Int String
    | Mark Int Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Add ->
            ( model ++ [ defaultItem ]
            , Cmd.none
            )

        Remove idx ->
            ( model
                |> List.indexedMap (\i item -> { i = i, item = item })
                |> List.filter (\indexedItem -> indexedItem.i /= idx)
                |> List.map (\indexedItem -> indexedItem.item)
            , Cmd.none
            )

        Change idx text ->
            ( useAt idx (\todo -> { todo | text = text }) model
            , Cmd.none
            )

        Mark idx done ->
            ( useAt idx (\todo -> { todo | done = done }) model
            , Cmd.none
            )

defaultItem : Todo
defaultItem =
    Todo "" False

useAt : Int -> (a -> a) -> List a -> List a
useAt idx fn items =
    List.indexedMap
        (\i item ->
            if i == idx then
                fn item

            else
                item
        )
        items

-- ---------------------------
-- VIEW
-- ---------------------------

viewItem : Int -> Todo -> Html Msg
viewItem idx todoItem =
    div
        [ class "input-group mt-2"
        ]
        [ div
            [ class "input-group-prepend"
            ]
            [ div
                [ class "input-group-text"
                ]
                [ input
                    [ type_ "checkbox"
                    , class ""
                    , checked todoItem.done
                    , onCheck (Mark idx)
                    ]
                    []
                ]
            ]
        , input
            [ value todoItem.text
            , class "form-control"
            , onInput (Change idx)
            , placeholder "Todo Item"
            ]
            []
        , div
            [ class "input-group-append"
            ]
            [ button
                [ onClick (Remove idx)
                , class "btn btn-danger"
                ]
                [ text "Remove"
                ]
            ]
        ]

view : Model -> Html Msg
view model =
    div
        [ class "card col-12 col-md-8 col-xl-6 mx-auto"
        ]
        [ div
            [ class "card-body"
            ]
            [ div
                [ class "container d-flex"
                ]
                [ h1
                    [ class "d-inline"
                    ]
                    [ text "Todo"
                    ]
                , div
                    [ class "ml-auto d-flex"
                    ]
                    [ button
                        [ onClick Add
                        , class "btn btn-primary my-auto"
                        ]
                        [ text "+"
                        ]
                    ]
                ]
            , div
                [
                ]
                (List.indexedMap viewItem model)
            ]
        ]

-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm Todo List"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
