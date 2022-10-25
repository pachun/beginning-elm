module TicTacToe2 exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Board BrowserInteraction
main =
    Browser.sandbox { init = emptyBoard, update = updateBoard, view = board }



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
updateBoard browserInteraction ticTacToeBoard =
    case browserInteraction of
        ClickedTopLeft ->
            updateBoardSquare ticTacToeBoard.topLeft { ticTacToeBoard | topLeft = turn ticTacToeBoard } ticTacToeBoard

        ClickedTopMiddle ->
            updateBoardSquare ticTacToeBoard.topMiddle { ticTacToeBoard | topMiddle = turn ticTacToeBoard } ticTacToeBoard

        ClickedTopRight ->
            updateBoardSquare ticTacToeBoard.topRight { ticTacToeBoard | topRight = turn ticTacToeBoard } ticTacToeBoard

        ClickedMiddleLeft ->
            updateBoardSquare ticTacToeBoard.middleLeft { ticTacToeBoard | middleLeft = turn ticTacToeBoard } ticTacToeBoard

        ClickedMiddle ->
            updateBoardSquare ticTacToeBoard.middle { ticTacToeBoard | middle = turn ticTacToeBoard } ticTacToeBoard

        ClickedMiddleRight ->
            updateBoardSquare ticTacToeBoard.middleRight { ticTacToeBoard | middleRight = turn ticTacToeBoard } ticTacToeBoard

        ClickedBottomLeft ->
            updateBoardSquare ticTacToeBoard.bottomLeft { ticTacToeBoard | bottomLeft = turn ticTacToeBoard } ticTacToeBoard

        ClickedBottomMiddle ->
            updateBoardSquare ticTacToeBoard.bottomMiddle { ticTacToeBoard | bottomMiddle = turn ticTacToeBoard } ticTacToeBoard

        ClickedBottomRight ->
            updateBoardSquare ticTacToeBoard.bottomRight { ticTacToeBoard | bottomRight = turn ticTacToeBoard } ticTacToeBoard

        ClickedReset ->
            emptyBoard


turn : Board -> BoardSquare
turn ticTacToeBoard =
    let
        boardSquareValues =
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
            boardSquareValues
                |> List.filter ((==) X)
                |> List.length

        numberOfOs =
            boardSquareValues
                |> List.filter ((==) O)
                |> List.length

        xsTurn =
            numberOfXs <= numberOfOs
    in
    if xsTurn then
        X

    else
        O


updateBoardSquare : BoardSquare -> Board -> Board -> Board
updateBoardSquare currentBoardSquare ticTacToeBoardWithChange ticTacToeBoardWithoutChange =
    if winner ticTacToeBoardWithoutChange == Empty && currentBoardSquare == Empty then
        ticTacToeBoardWithChange

    else
        ticTacToeBoardWithoutChange


type alias Winner =
    BoardSquare


winner : Board -> Winner
winner ticTacToeBoard =
    let
        hasTopRowWinner =
            ticTacToeBoard.topLeft
                /= Empty
                && ticTacToeBoard.topLeft
                == ticTacToeBoard.topMiddle
                && ticTacToeBoard.topMiddle
                == ticTacToeBoard.topRight

        hasMiddleRowWinner =
            ticTacToeBoard.middleLeft
                /= Empty
                && ticTacToeBoard.middleLeft
                == ticTacToeBoard.middle
                && ticTacToeBoard.middle
                == ticTacToeBoard.middleRight

        hasBottomRowWinner =
            ticTacToeBoard.bottomLeft
                /= Empty
                && ticTacToeBoard.bottomLeft
                == ticTacToeBoard.bottomMiddle
                && ticTacToeBoard.bottomMiddle
                == ticTacToeBoard.bottomRight

        hasLeftColumnWinner =
            ticTacToeBoard.topLeft
                /= Empty
                && ticTacToeBoard.topLeft
                == ticTacToeBoard.middleLeft
                && ticTacToeBoard.middleLeft
                == ticTacToeBoard.bottomLeft

        hasMiddleColumnWinner =
            ticTacToeBoard.topMiddle
                /= Empty
                && ticTacToeBoard.topMiddle
                == ticTacToeBoard.middle
                && ticTacToeBoard.middle
                == ticTacToeBoard.bottomMiddle

        hasRightColumnWinner =
            ticTacToeBoard.topRight
                /= Empty
                && ticTacToeBoard.topRight
                == ticTacToeBoard.middleRight
                && ticTacToeBoard.middleRight
                == ticTacToeBoard.bottomRight

        hasForwardSlashWinner =
            ticTacToeBoard.bottomLeft
                /= Empty
                && ticTacToeBoard.bottomLeft
                == ticTacToeBoard.middle
                && ticTacToeBoard.middle
                == ticTacToeBoard.topRight

        hasBackSlashWinner =
            ticTacToeBoard.topLeft
                /= Empty
                && ticTacToeBoard.topLeft
                == ticTacToeBoard.middle
                && ticTacToeBoard.middle
                == ticTacToeBoard.bottomRight
    in
    if hasTopRowWinner then
        ticTacToeBoard.topLeft

    else if hasMiddleRowWinner then
        ticTacToeBoard.middleLeft

    else if hasBottomRowWinner then
        ticTacToeBoard.bottomLeft

    else if hasLeftColumnWinner then
        ticTacToeBoard.topLeft

    else if hasMiddleColumnWinner then
        ticTacToeBoard.topMiddle

    else if hasRightColumnWinner then
        ticTacToeBoard.topRight

    else if hasForwardSlashWinner then
        ticTacToeBoard.bottomLeft

    else if hasBackSlashWinner then
        ticTacToeBoard.topLeft

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


board : Board -> Html BrowserInteraction
board ticTacToeBoard =
    Html.div []
        [ Html.table [ style "border" "1px solid #000", style "border-collapse" "collapse" ]
            [ Html.tr []
                [ boardSquareHtml ClickedTopLeft .topLeft ticTacToeBoard
                , boardSquareHtml ClickedTopMiddle .topMiddle ticTacToeBoard
                , boardSquareHtml ClickedTopRight .topRight ticTacToeBoard
                ]
            , Html.tr []
                [ boardSquareHtml ClickedMiddleLeft .middleLeft ticTacToeBoard
                , boardSquareHtml ClickedMiddle .middle ticTacToeBoard
                , boardSquareHtml ClickedMiddleRight .middleRight ticTacToeBoard
                ]
            , Html.tr []
                [ boardSquareHtml ClickedBottomLeft .bottomLeft ticTacToeBoard
                , boardSquareHtml ClickedBottomMiddle .bottomMiddle ticTacToeBoard
                , boardSquareHtml ClickedBottomRight .bottomRight ticTacToeBoard
                ]
            ]
        , Html.text ("Winner: " ++ Debug.toString (winner ticTacToeBoard))
        , Html.button [ onClick ClickedReset ] [ Html.text "Reset" ]
        ]
