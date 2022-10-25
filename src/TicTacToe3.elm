module TicTacToe3 exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox
        { init = emptyBoard 3
        , update = updateBoard
        , view = boardHtml
        }



-- MODEL


type BoardSquare
    = X
    | O
    | Empty


type alias Board =
    Array (Array BoardSquare)


emptyBoard : Int -> Board
emptyBoard boardSize =
    Array.fromList [ Array.fromList [ X ] ]



-- UPDATE


type alias BoardSquarePosition =
    ( Int, Int )


type BrowserInteraction
    = Clicked BoardSquarePosition


updateBoard : BrowserInteraction -> Board -> Board
updateBoard browserInteraction board =
    board



-- VIEW


boardHtml : Board -> Html BrowserInteraction
boardHtml board =
    Html.div [] []
