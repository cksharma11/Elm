module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)

main = sandbox { init = init, view = view, update = update}

init : { todo: String, todos : List String }
init = { todo = "", todos = [] }

type Msg
    = UpdateTodo String
    | AddTodo

type alias Model =
    { todo : String
    , todos : List String
    }

todoView : String -> Html Msg
todoView todo = 
  div [] [ text todo ]

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTodo todoText ->
      { model | todo = todoText }

    AddTodo ->
      { model | todo = "", todos = model.todos ++ [ model.todo ] }


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput UpdateTodo, value model.todo ] []
        , button [ onClick AddTodo ] [ text "Add Todo" ]
        , div [] (List.map todoView model.todos)
        ]