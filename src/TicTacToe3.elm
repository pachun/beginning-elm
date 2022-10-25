module TicTacToe3 exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox
        { init = initialBoard
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
    Array.repeat boardSize (Array.repeat boardSize Empty)


initialBoardSize =
    3


initialBoard =
    emptyBoard initialBoardSize



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
    Html.div []
        [ Html.table [ style "border" "1px solid #000", style "border-collapse" "collapse" ]
            (Array.toList (Array.map (\boardRow -> Html.tr [] (Array.toList (Array.map (\boardCell -> Html.td [] []) boardRow))) board))
        ]
