module TicTacToe exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (..)

main = sandbox {init = init, view = view, update = update }

type alias Board = List Symbol

winningMoves : List Moves
winningMoves =  [[0,1,2]
                ,[3,4,5]
                ,[6,7,8]
                ,[0,4,8]
                ,[2,4,6]
                ,[0,3,6]
                ,[1,4,7]
                ,[2,5,8]]

type Msg
    = PlaceSymbol Int 

type Symbol 
    = X
    | O
    | Empty

type alias Moves = List Int

type alias Status =
        {hasWon: Bool
        ,winner: Symbol}

type alias Model = 
        {board : Board
        ,currentSymbol : Symbol
        ,currentPlayer : Moves
        ,otherPlayer : Moves
        ,status : Status
        }

init : Model
init = { board = [Empty, Empty, Empty,Empty, Empty, Empty,Empty, Empty, Empty] 
        ,currentSymbol = X
        ,currentPlayer = []
        ,otherPlayer = []
        ,status = {hasWon = False, winner = Empty}
        }

updateTurn : Symbol -> Symbol
updateTurn s =
    case s of
        X -> O
        _ -> X          

update : Msg -> Model -> Model
update msg model =
    case msg of
        PlaceSymbol position ->
            {board = updateInList position model.board model.currentSymbol
            ,currentPlayer =  model.otherPlayer
            ,otherPlayer = model.currentPlayer ++ [position]
            ,status = hasWon (model.currentPlayer ++ [position]) model.currentSymbol
            ,currentSymbol = updateTurn model.currentSymbol
            }

cell : Bool -> Int -> Symbol -> Html Msg
cell won position symbol =
    button (if symbol == Empty && won == False
            then [onClick (PlaceSymbol position)] 
            else []) [ text (Debug.toString symbol) ]

view : Model -> Html Msg
view model =
    div []
      [div [style "display" "grid"
        ,style "grid-template-columns" "auto auto auto"
        ,style "width" "150px"] 
        (indexedMap (cell model.status.hasWon) model.board)
      ,div [] (if model.status.hasWon then [text ((Debug.toString model.status.winner) ++ " has won!")] else [])]

hasWon : List Int -> Symbol -> Status
hasWon moves symbol =
    {hasWon = List.any (\wm -> List.all (\e -> member e moves) wm) winningMoves
    ,winner = symbol}

setNewVal : Int -> Symbol -> Int -> Symbol -> Symbol
setNewVal pos newSymbol currentPos currentSymbol =
    if pos == currentPos
    then newSymbol
    else currentSymbol

updateInList : Int -> Board -> Symbol -> Board
updateInList pos board newSymbol =
    indexedMap (setNewVal pos newSymbol) board