module TicTacToe exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (..)

main = sandbox {init = init, view = view, update = update }

type alias Board = List Symbol

winningMoves =  [[1,2,3]
                ,[4,5,6]
                ,[7,8,9]
                ,[1,5,9]
                ,[3,5,7]
                ,[1,4,7]
                ,[2,5,8]
                ,[3,6,9]]

type Msg
    = PlaceSymbol Int 

type Symbol 
    = X
    | O
    | Empty

type alias Moves = List Int
type alias Player = 
        {symbol: Symbol
        ,moves:Moves}

type alias Model = 
        {board : Board
        ,currentSymbol : Symbol
        ,x : Player
        ,o : Player
        ,won : Bool
        }

init : Model
init = { board = [Empty, Empty, Empty,Empty, Empty, Empty,Empty, Empty, Empty] 
        ,currentSymbol = X
        ,x = {symbol =  X, moves =  []}
        ,o = {symbol =  O, moves  = []}
        ,won = False
        }

updateTurn : Symbol -> Symbol
updateTurn s =
    case s of
        X -> O
        _ -> X

storeMove : Model -> Int -> Player
storeMove model pos =
    if model.currentSymbol == X
    then {symbol=X, moves = model.x.moves ++ [pos]}
    else {symbol=O, moves = model.x.moves ++ [pos]}
                

update : Msg -> Model -> Model
update msg model =
    case msg of
        PlaceSymbol position ->
            {board = updateInList position model.board model.currentSymbol
            ,x = storeMove model position
            ,o = storeMove model position
            ,won = hasWon (if X == model.currentSymbol then model.x.moves else model.o.moves)
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
      ,div [] (if model.won then [text "Player has won"] else [])]

hasWon : List Int -> Bool
hasWon moves =
    List.any (\wm -> List.all (\e -> member e moves) wm) winningMoves

setNewVal : Int -> Symbol -> Int -> Symbol -> Symbol
setNewVal pos newSymbol currentPos currentSymbol =
    if pos == currentPos
    then newSymbol
    else currentSymbol

updateInList : Int -> Board -> Symbol -> Board
updateInList pos board newSymbol =
    indexedMap (setNewVal pos newSymbol) board