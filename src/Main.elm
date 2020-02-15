module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (autofocus, class, value)
import Html.Events exposing (onClick, onInput)

type Msg
    = UpdateTodo String
    | AddTodo

type alias Model =
    { text : String
    , todos : List String
    }

view : Model -> Html Msg
view model =
    div [ class "text-center" ]
        [ input [ onInput UpdateTodo, value model.text, autofocus True ] []
        , button [ onClick AddTodo ] [ text "Add Todo" ]
        , div [] (List.map todoView model.todos)
        ]

todoView : String -> Html Msg
todoView todo = 
  div [] [ text todo ]

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTodo todoText ->
      { model | text = todoText }

    AddTodo ->
      { model | text = "", todos = model.todos ++ [ model.text ] }

main : Program () Model Msg
main =
    sandbox
        { init = { text = "", todos = [] }
        , view = view
        , update = update
        }