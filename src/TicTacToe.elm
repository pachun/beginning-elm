module TicTacToe exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = board, update = update, view = view }



-- model


type BoardSquareValue
    = X
    | O
    | Empty


type BoardSquarePosition
    = TopLeft
    | TopMiddle
    | TopRight
    | MiddleLeft
    | Middle
    | MiddleRight
    | BottomLeft
    | BottomMiddle
    | BottomRight



-- this should be some sort of set, not a list
-- it should require one and only one of each of the BoardSquarePosition types


type alias BoardSquare =
    ( BoardSquarePosition, BoardSquareValue )


type alias Board =
    List BoardSquare


board : Board
board =
    [ ( TopLeft, Empty )
    , ( TopMiddle, Empty )
    , ( TopRight, Empty )
    , ( MiddleLeft, Empty )
    , ( Middle, Empty )
    , ( MiddleRight, Empty )
    , ( BottomLeft, Empty )
    , ( BottomMiddle, Empty )
    , ( BottomRight, Empty )
    ]


type Turn
    = XsTurn
    | OsTurn
    | GameOver


turn board1 =
    let
        plays =
            board1
                |> List.unzip
                |> Tuple.second

        firstTurn =
            plays
                |> List.all (\play -> play == Empty)

        gameOver =
            plays
                |> List.any (\play -> play == Empty)
                |> not

        numberOfXsTurns =
            plays
                |> List.filter (\play -> play == X)
                |> List.length

        numberOfOsTurns =
            plays
                |> List.filter (\play -> play == O)
                |> List.length
    in
    if gameOver then
        GameOver

    else if firstTurn || numberOfXsTurns < numberOfOsTurns then
        XsTurn

    else
        OsTurn



-- update


type Msg
    = BoardSquarePosition


update : BoardSquarePosition -> Board -> Board
update boardSquarePosition board2 =
    let
        nextTurn =
            turn board2

        nextTurnBoardSquareValue =
            if nextTurn == XsTurn then
                X

            else
                O
    in
    List.map
        (\boardSquare ->
            if Tuple.first boardSquare == boardSquarePosition then
                ( Tuple.first boardSquare, nextTurnBoardSquareValue )

            else
                boardSquare
        )
        board2


boardValue : BoardSquarePosition -> Board -> String
boardValue boardSquarePosition board3 =
    let
        boardSquare = Tuple.second( List.head (List.filter (\s -> Tuple.first s == boardSquarePosition) board3)
    in
    if boardSquarePosition == X then
        "X"

    else
        "O"



-- VIEW


view model =
    Html.div []
        [ Html.text (Debug.toString (turn board))
        , Html.table [ style "border" "1px solid #000", style "border-collapse" "collapse" ]
            [ Html.tr []
                [ Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button [ onClick TopLeft, style "width" "100px", style "height" "100px", style "background" "none", style "border" "none" ]
                        [ Html.span [ style "font-size" "30pt" ] [ Html.text (boardValue TopLeft) ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button [ style "width" "100px", style "height" "100px", style "background" "none", style "border" "none" ]
                        [ Html.span [ style "font-size" "30pt" ] [ Html.text "X" ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button [ style "width" "100px", style "height" "100px", style "background" "none", style "border" "none" ]
                        [ Html.span [ style "font-size" "30pt" ] [ Html.text "O" ]
                        ]
                    ]
                ]
            , Html.tr []
                [ Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button [ style "width" "100px", style "height" "100px", style "background" "none", style "border" "none" ]
                        [ Html.span [ style "font-size" "30pt" ] [ Html.text "X" ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button [ style "width" "100px", style "height" "100px", style "background" "none", style "border" "none" ]
                        [ Html.span [ style "font-size" "30pt" ] [ Html.text "X" ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button [ style "width" "100px", style "height" "100px", style "background" "none", style "border" "none" ]
                        [ Html.span [ style "font-size" "30pt" ] [ Html.text "O" ]
                        ]
                    ]
                ]
            , Html.tr []
                [ Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button [ style "width" "100px", style "height" "100px", style "background" "none", style "border" "none" ]
                        [ Html.span [ style "font-size" "30pt" ] [ Html.text "X" ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button [ style "width" "100px", style "height" "100px", style "background" "none", style "border" "none" ]
                        [ Html.span [ style "font-size" "30pt" ] [ Html.text "X" ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button [ style "width" "100px", style "height" "100px", style "background" "none", style "border" "none" ]
                        [ Html.span [ style "font-size" "30pt" ] [ Html.text "O" ]
                        ]
                    ]
                ]
            ]
        ]
