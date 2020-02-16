module TicTacToe exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (..)

main = sandbox {init = init, view = view, update = update }

type alias Board = List Symbol

type Msg
    = PlaceSymbol Int 

type Symbol 
    = X
    | O
    | Empty
    

type alias Model = 
        {board : Board
        ,currentSymbol : Symbol
        }

init : Model
init = { board = [Empty, Empty, Empty,Empty, Empty, Empty,Empty, Empty, Empty] 
        ,currentSymbol = X
        }

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
    div []
      [div [style "display" "grid"
        ,style "grid-template-columns" "auto auto auto"
        ,style "width" "150px"] 
        (indexedMap cell model.board)
      ,div [] (if hasWon model.board then [text "Player has won"] else [])]

hasWon : Board -> Bool
hasWon board =
    False

updateTurn : Symbol -> Symbol
updateTurn s =
    if s == X
    then O
    else X

setNewVal : Int -> Symbol -> Int -> Symbol -> Symbol
setNewVal pos newSymbol currentPos currentSymbol =
    if pos == currentPos
    then newSymbol
    else currentSymbol

updateInList : Int -> Board -> Symbol -> Board
updateInList pos board newSymbol =
    indexedMap (setNewVal pos newSymbol) board