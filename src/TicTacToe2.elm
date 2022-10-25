module TicTacToe2 exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main : Program () TicTacToeBoard Action
main =
    Browser.sandbox { init = emptyTicTacToeBoard, update = update, view = view }



-- MODEL


type BoardSquareValue
    = X
    | O
    | Empty


type alias TicTacToeBoard =
    { topLeft : BoardSquareValue
    , topMiddle : BoardSquareValue
    , topRight : BoardSquareValue
    , middleLeft : BoardSquareValue
    , middle : BoardSquareValue
    , middleRight : BoardSquareValue
    , bottomLeft : BoardSquareValue
    , bottomMiddle : BoardSquareValue
    , bottomRight : BoardSquareValue
    }


emptyTicTacToeBoard : TicTacToeBoard
emptyTicTacToeBoard =
    TicTacToeBoard Empty Empty Empty Empty Empty Empty Empty Empty Empty



-- UPDATE


type Action
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


turn : TicTacToeBoard -> BoardSquareValue
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


updateBoardSquareValue : BoardSquareValue -> TicTacToeBoard -> TicTacToeBoard -> TicTacToeBoard
updateBoardSquareValue currentBoardSquareValue ticTacToeBoardWithChange ticTacToeBoardWithoutChange =
    if winner ticTacToeBoardWithoutChange == Empty && currentBoardSquareValue == Empty then
        ticTacToeBoardWithChange

    else
        ticTacToeBoardWithoutChange


update : Action -> TicTacToeBoard -> TicTacToeBoard
update turnAction ticTacToeBoard =
    case turnAction of
        ClickedTopLeft ->
            updateBoardSquareValue ticTacToeBoard.topLeft { ticTacToeBoard | topLeft = turn ticTacToeBoard } ticTacToeBoard

        ClickedTopMiddle ->
            updateBoardSquareValue ticTacToeBoard.topMiddle { ticTacToeBoard | topMiddle = turn ticTacToeBoard } ticTacToeBoard

        ClickedTopRight ->
            updateBoardSquareValue ticTacToeBoard.topRight { ticTacToeBoard | topRight = turn ticTacToeBoard } ticTacToeBoard

        ClickedMiddleLeft ->
            updateBoardSquareValue ticTacToeBoard.middleLeft { ticTacToeBoard | middleLeft = turn ticTacToeBoard } ticTacToeBoard

        ClickedMiddle ->
            updateBoardSquareValue ticTacToeBoard.middle { ticTacToeBoard | middle = turn ticTacToeBoard } ticTacToeBoard

        ClickedMiddleRight ->
            updateBoardSquareValue ticTacToeBoard.middleRight { ticTacToeBoard | middleRight = turn ticTacToeBoard } ticTacToeBoard

        ClickedBottomLeft ->
            updateBoardSquareValue ticTacToeBoard.bottomLeft { ticTacToeBoard | bottomLeft = turn ticTacToeBoard } ticTacToeBoard

        ClickedBottomMiddle ->
            updateBoardSquareValue ticTacToeBoard.bottomMiddle { ticTacToeBoard | bottomMiddle = turn ticTacToeBoard } ticTacToeBoard

        ClickedBottomRight ->
            updateBoardSquareValue ticTacToeBoard.bottomRight { ticTacToeBoard | bottomRight = turn ticTacToeBoard } ticTacToeBoard

        ClickedReset ->
            emptyTicTacToeBoard


winner : TicTacToeBoard -> BoardSquareValue
winner ticTacToeBoard =
    let
        topRowWinner =
            ticTacToeBoard.topLeft
                /= Empty
                && ticTacToeBoard.topLeft
                == ticTacToeBoard.topMiddle
                && ticTacToeBoard.topMiddle
                == ticTacToeBoard.topRight

        middleRowWinner =
            ticTacToeBoard.middleLeft
                /= Empty
                && ticTacToeBoard.middleLeft
                == ticTacToeBoard.middle
                && ticTacToeBoard.middle
                == ticTacToeBoard.middleRight

        bottomRowWinner =
            ticTacToeBoard.bottomLeft
                /= Empty
                && ticTacToeBoard.bottomLeft
                == ticTacToeBoard.bottomMiddle
                && ticTacToeBoard.bottomMiddle
                == ticTacToeBoard.bottomRight

        leftColumnWinner =
            ticTacToeBoard.topLeft
                /= Empty
                && ticTacToeBoard.topLeft
                == ticTacToeBoard.middleLeft
                && ticTacToeBoard.middleLeft
                == ticTacToeBoard.bottomLeft

        middleColumnWinner =
            ticTacToeBoard.topMiddle
                /= Empty
                && ticTacToeBoard.topMiddle
                == ticTacToeBoard.middle
                && ticTacToeBoard.middle
                == ticTacToeBoard.bottomMiddle

        rightColumnWinner =
            ticTacToeBoard.topRight
                /= Empty
                && ticTacToeBoard.topRight
                == ticTacToeBoard.middleRight
                && ticTacToeBoard.middleRight
                == ticTacToeBoard.bottomRight

        forwardSlashWinner =
            ticTacToeBoard.bottomLeft
                /= Empty
                && ticTacToeBoard.bottomLeft
                == ticTacToeBoard.middle
                && ticTacToeBoard.middle
                == ticTacToeBoard.topRight

        backSlashWinner =
            ticTacToeBoard.topLeft
                /= Empty
                && ticTacToeBoard.topLeft
                == ticTacToeBoard.middle
                && ticTacToeBoard.middle
                == ticTacToeBoard.bottomRight
    in
    if topRowWinner then
        ticTacToeBoard.topLeft

    else if middleRowWinner then
        ticTacToeBoard.middleLeft

    else if bottomRowWinner then
        ticTacToeBoard.bottomLeft

    else if leftColumnWinner then
        ticTacToeBoard.topLeft

    else if middleColumnWinner then
        ticTacToeBoard.topMiddle

    else if rightColumnWinner then
        ticTacToeBoard.topRight

    else if forwardSlashWinner then
        ticTacToeBoard.bottomLeft

    else if backSlashWinner then
        ticTacToeBoard.topLeft

    else
        Empty



-- VIEW


boardSquareValue : BoardSquareValue -> String
boardSquareValue value =
    case value of
        X ->
            "X"

        O ->
            "O"

        Empty ->
            ""


boardSquareHtml : Action -> (TicTacToeBoard -> BoardSquareValue) -> TicTacToeBoard -> Html Action
boardSquareHtml action cellPosition ticTacToeBoard =
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
                [ ticTacToeBoard |> cellPosition |> boardSquareValue |> Html.text ]
            ]
        ]


view : TicTacToeBoard -> Html Action
view ticTacToeBoard =
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
