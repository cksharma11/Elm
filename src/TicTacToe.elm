module TicTacToe exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, value, style)
import Html.Events exposing (onClick, onInput)
import List exposing (..)

main = sandbox {init = init, view = view, update = update }

type Msg
    = PlaceSymbol Int 

type Symbol 
    = X
    | O
    | Empty
    
type alias Model = 
    {board : List Symbol
    ,currentSymbol : Symbol
    }

init : Model
init = { board = [Empty, Empty, Empty,Empty, Empty, Empty,Empty, Empty, Empty] 
        ,currentSymbol = X
        }

updateTurn : Symbol -> Symbol
updateTurn s =
    if s == X
    then O
    else X

update : Msg -> Model -> Model
update msg model =
    case msg of
        PlaceSymbol position ->
            {board = updateInList position model.board model.currentSymbol
            ,currentSymbol = updateTurn model.currentSymbol
            }

cell : Int -> Symbol -> Html Msg
cell position symbol =
    button (if symbol == Empty 
            then [onClick (PlaceSymbol position)] 
            else []) [ text (Debug.toString symbol) ]

view : Model -> Html Msg
view model =
    div [style "display" "grid"
        ,style "grid-template-columns" "auto auto auto"
        ,style "width" "150px"]
      (indexedMap cell model.board)

setNewVal : Int -> Symbol -> Int -> Symbol -> Symbol
setNewVal p ns i cs =
    if i == p && cs == Empty
    then ns
    else cs

updateInList : Int -> List Symbol -> Symbol -> List Symbol
updateInList pos board ns =
    indexedMap (setNewVal pos ns) board