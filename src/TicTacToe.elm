module TicTacToe exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import List exposing (..)

main = sandbox {init = init, view = view, update = update }

type Msg
    = UpdateTurn
    | MakeMove

type Symbol 
    = X
    | O
    | Empty
    
type alias Model = 
    { board : List Symbol
    }

init : Model
init = { board = [Empty, Empty, Empty,Empty, Empty, Empty,Empty, Empty, Empty] }

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTurn ->
            init

        MakeMove ->
            init

cell : Symbol -> Html Msg
cell symbol =
    button [ onClick MakeMove ] [ text "X" ]

view : Model -> Html Msg
view model =
    div []
      (map cell model.board)