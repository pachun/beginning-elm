module TicTacToe2 exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Board BrowserInteraction
main =
    Browser.sandbox { init = emptyBoard, update = updateBoard, view = boardHtml }



-- MODEL


type BoardSquare
    = X
    | O
    | Empty


type alias Board =
    { topLeft : BoardSquare
    , topMiddle : BoardSquare
    , topRight : BoardSquare
    , middleLeft : BoardSquare
    , middle : BoardSquare
    , middleRight : BoardSquare
    , bottomLeft : BoardSquare
    , bottomMiddle : BoardSquare
    , bottomRight : BoardSquare
    }


emptyBoard : Board
emptyBoard =
    Board Empty Empty Empty Empty Empty Empty Empty Empty Empty



-- UPDATE


type BrowserInteraction
    = ClickedTopLeft
    | ClickedTopMiddle
    | ClickedTopRight
    | ClickedMiddleLeft
    | ClickedMiddle
    | ClickedMiddleRight
    | ClickedBottomLeft
    | ClickedBottomMiddle
    | ClickedBottomRight
    | ClickedReset


updateBoard : BrowserInteraction -> Board -> Board
updateBoard browserInteraction board =
    case browserInteraction of
        ClickedTopLeft ->
            updateBoardSquare .topLeft { board | topLeft = board |> currentTurn |> boardSquareFromTurn } board

        ClickedTopMiddle ->
            updateBoardSquare .topMiddle { board | topMiddle = board |> currentTurn |> boardSquareFromTurn } board

        ClickedTopRight ->
            updateBoardSquare .topRight { board | topRight = board |> currentTurn |> boardSquareFromTurn } board

        ClickedMiddleLeft ->
            updateBoardSquare .middleLeft { board | middleLeft = board |> currentTurn |> boardSquareFromTurn } board

        ClickedMiddle ->
            updateBoardSquare .middle { board | middle = board |> currentTurn |> boardSquareFromTurn } board

        ClickedMiddleRight ->
            updateBoardSquare .middleRight { board | middleRight = board |> currentTurn |> boardSquareFromTurn } board

        ClickedBottomLeft ->
            updateBoardSquare .bottomLeft { board | bottomLeft = board |> currentTurn |> boardSquareFromTurn } board

        ClickedBottomMiddle ->
            updateBoardSquare .bottomMiddle { board | bottomMiddle = board |> currentTurn |> boardSquareFromTurn } board

        ClickedBottomRight ->
            updateBoardSquare .bottomRight { board | bottomRight = board |> currentTurn |> boardSquareFromTurn } board

        ClickedReset ->
            emptyBoard


type Turn
    = XsTurn
    | OsTurn


currentTurn : Board -> Turn
currentTurn ticTacToeBoard =
    let
        boardSquares =
            [ .topLeft ticTacToeBoard
            , .topMiddle ticTacToeBoard
            , .topRight ticTacToeBoard
            , .middleLeft ticTacToeBoard
            , .middle ticTacToeBoard
            , .middleRight ticTacToeBoard
            , .bottomLeft ticTacToeBoard
            , .bottomMiddle ticTacToeBoard
            , .bottomRight ticTacToeBoard
            ]

        numberOfXs =
            boardSquares
                |> List.filter ((==) X)
                |> List.length

        numberOfOs =
            boardSquares
                |> List.filter ((==) O)
                |> List.length

        xsTurn =
            numberOfXs <= numberOfOs
    in
    if xsTurn then
        XsTurn

    else
        OsTurn


boardSquareFromTurn : Turn -> BoardSquare
boardSquareFromTurn turn =
    case turn of
        XsTurn ->
            X

        OsTurn ->
            O


updateBoardSquare : (Board -> BoardSquare) -> Board -> Board -> Board
updateBoardSquare boardSquareFromBoard ticTacToeBoardWithChange ticTacToeBoardWithoutChange =
    if winner ticTacToeBoardWithoutChange == Empty && boardSquareFromBoard ticTacToeBoardWithoutChange == Empty then
        ticTacToeBoardWithChange

    else
        ticTacToeBoardWithoutChange


type alias Winner =
    BoardSquare


isWinningRow : (Board -> BoardSquare) -> (Board -> BoardSquare) -> (Board -> BoardSquare) -> Board -> Bool
isWinningRow square1 square2 square3 board =
    square1 board /= Empty && square1 board == square2 board && square2 board == square3 board


winner : Board -> Winner
winner board =
    if board |> isWinningRow .topLeft .topMiddle .topRight then
        board.topLeft

    else if board |> isWinningRow .middleLeft .middle .middleRight then
        board.middleLeft

    else if board |> isWinningRow .bottomLeft .bottomMiddle .bottomRight then
        board.bottomLeft

    else if board |> isWinningRow .topLeft .middleLeft .bottomLeft then
        board.topLeft

    else if board |> isWinningRow .topMiddle .middle .bottomMiddle then
        board.topMiddle

    else if board |> isWinningRow .topRight .middleRight .bottomRight then
        board.topRight

    else if board |> isWinningRow .bottomLeft .middle .topRight then
        board.bottomLeft

    else if board |> isWinningRow .topLeft .middle .bottomRight then
        board.topLeft

    else
        Empty



-- VIEW


boardSquareAsString : BoardSquare -> String
boardSquareAsString value =
    case value of
        X ->
            "X"

        O ->
            "O"

        Empty ->
            ""


boardSquareHtml : BrowserInteraction -> (Board -> BoardSquare) -> Board -> Html BrowserInteraction
boardSquareHtml action boardSquare ticTacToeBoard =
    Html.td
        [ style "border" "1px solid #000"
        , style "border-collapse" "collapse"
        ]
        [ Html.button
            [ style "width" "100px"
            , style "height" "100px"
            , style "background" "none"
            , style "border" "none"
            , onClick action
            ]
            [ Html.span
                [ style "font-size" "30pt" ]
                [ ticTacToeBoard |> boardSquare |> boardSquareAsString |> Html.text ]
            ]
        ]


boardHtml : Board -> Html BrowserInteraction
boardHtml board =
    Html.div []
        [ Html.table [ style "border" "1px solid #000", style "border-collapse" "collapse" ]
            [ Html.tr []
                [ boardSquareHtml ClickedTopLeft .topLeft board
                , boardSquareHtml ClickedTopMiddle .topMiddle board
                , boardSquareHtml ClickedTopRight .topRight board
                ]
            , Html.tr []
                [ boardSquareHtml ClickedMiddleLeft .middleLeft board
                , boardSquareHtml ClickedMiddle .middle board
                , boardSquareHtml ClickedMiddleRight .middleRight board
                ]
            , Html.tr []
                [ boardSquareHtml ClickedBottomLeft .bottomLeft board
                , boardSquareHtml ClickedBottomMiddle .bottomMiddle board
                , boardSquareHtml ClickedBottomRight .bottomRight board
                ]
            ]
        , Html.text ("Winner: " ++ Debug.toString (winner board))
        , Html.button [ onClick ClickedReset ] [ Html.text "Reset" ]
        ]
