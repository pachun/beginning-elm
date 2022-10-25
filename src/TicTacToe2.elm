module TicTacToe2 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



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


init : TicTacToeBoard
init =
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


update : Action -> TicTacToeBoard -> TicTacToeBoard
update turnAction ticTacToeBoard =
    case turnAction of
        ClickedTopLeft ->
            if
                winner ticTacToeBoard
                    == Empty
                    && .topLeft ticTacToeBoard
                    == Empty
            then
                { ticTacToeBoard | topLeft = turn ticTacToeBoard }

            else
                ticTacToeBoard

        ClickedTopMiddle ->
            if
                winner ticTacToeBoard
                    == Empty
                    && .topMiddle ticTacToeBoard
                    == Empty
            then
                { ticTacToeBoard | topMiddle = turn ticTacToeBoard }

            else
                ticTacToeBoard

        ClickedTopRight ->
            if
                winner ticTacToeBoard
                    == Empty
                    && .topRight ticTacToeBoard
                    == Empty
            then
                { ticTacToeBoard | topRight = turn ticTacToeBoard }

            else
                ticTacToeBoard

        ClickedMiddleLeft ->
            if
                winner ticTacToeBoard
                    == Empty
                    && .middleLeft ticTacToeBoard
                    == Empty
            then
                { ticTacToeBoard | middleLeft = turn ticTacToeBoard }

            else
                ticTacToeBoard

        ClickedMiddle ->
            if
                winner ticTacToeBoard
                    == Empty
                    && .middle ticTacToeBoard
                    == Empty
            then
                { ticTacToeBoard | middle = turn ticTacToeBoard }

            else
                ticTacToeBoard

        ClickedMiddleRight ->
            if
                winner ticTacToeBoard
                    == Empty
                    && .middleRight ticTacToeBoard
                    == Empty
            then
                { ticTacToeBoard | middleRight = turn ticTacToeBoard }

            else
                ticTacToeBoard

        ClickedBottomLeft ->
            if
                winner ticTacToeBoard
                    == Empty
                    && .bottomLeft ticTacToeBoard
                    == Empty
            then
                { ticTacToeBoard | bottomLeft = turn ticTacToeBoard }

            else
                ticTacToeBoard

        ClickedBottomMiddle ->
            if
                winner ticTacToeBoard
                    == Empty
                    && .bottomMiddle ticTacToeBoard
                    == Empty
            then
                { ticTacToeBoard | bottomMiddle = turn ticTacToeBoard }

            else
                ticTacToeBoard

        ClickedBottomRight ->
            if
                winner ticTacToeBoard
                    == Empty
                    && .bottomRight ticTacToeBoard
                    == Empty
            then
                { ticTacToeBoard | bottomRight = turn ticTacToeBoard }

            else
                ticTacToeBoard

        ClickedReset ->
            init


type alias Winner =
    BoardSquareValue


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
        ticTacToeBoard.middleLeft

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


view ticTacToeBoard =
    Html.div []
        [ Html.table [ style "border" "1px solid #000", style "border-collapse" "collapse" ]
            [ Html.tr []
                [ Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button
                        [ style "width" "100px"
                        , style "height" "100px"
                        , style "background" "none"
                        , style "border" "none"
                        , onClick ClickedTopLeft
                        ]
                        [ Html.span
                            [ style "font-size" "30pt" ]
                            [ ticTacToeBoard |> .topLeft |> boardSquareValue |> Html.text ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button
                        [ style "width" "100px"
                        , style "height" "100px"
                        , style "background" "none"
                        , style "border" "none"
                        , onClick ClickedTopMiddle
                        ]
                        [ Html.span
                            [ style "font-size" "30pt" ]
                            [ ticTacToeBoard |> .topMiddle |> boardSquareValue |> Html.text ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button
                        [ style "width" "100px"
                        , style "height" "100px"
                        , style "background" "none"
                        , style "border" "none"
                        , onClick ClickedTopRight
                        ]
                        [ Html.span
                            [ style "font-size" "30pt" ]
                            [ ticTacToeBoard |> .topRight |> boardSquareValue |> Html.text ]
                        ]
                    ]
                ]
            , Html.tr []
                [ Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button
                        [ style "width" "100px"
                        , style "height" "100px"
                        , style "background" "none"
                        , style "border" "none"
                        , onClick ClickedMiddleLeft
                        ]
                        [ Html.span
                            [ style "font-size" "30pt" ]
                            [ ticTacToeBoard |> .middleLeft |> boardSquareValue |> Html.text ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button
                        [ style "width" "100px"
                        , style "height" "100px"
                        , style "background" "none"
                        , style "border" "none"
                        , onClick ClickedMiddle
                        ]
                        [ Html.span
                            [ style "font-size" "30pt" ]
                            [ ticTacToeBoard |> .middle |> boardSquareValue |> Html.text ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button
                        [ style "width" "100px"
                        , style "height" "100px"
                        , style "background" "none"
                        , style "border" "none"
                        , onClick ClickedMiddleRight
                        ]
                        [ Html.span
                            [ style "font-size" "30pt" ]
                            [ ticTacToeBoard |> .middleRight |> boardSquareValue |> Html.text ]
                        ]
                    ]
                ]
            , Html.tr []
                [ Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button
                        [ style "width" "100px"
                        , style "height" "100px"
                        , style "background" "none"
                        , style "border" "none"
                        , onClick ClickedBottomLeft
                        ]
                        [ Html.span
                            [ style "font-size" "30pt" ]
                            [ ticTacToeBoard |> .bottomLeft |> boardSquareValue |> Html.text ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button
                        [ style "width" "100px"
                        , style "height" "100px"
                        , style "background" "none"
                        , style "border" "none"
                        , onClick ClickedBottomMiddle
                        ]
                        [ Html.span
                            [ style "font-size" "30pt" ]
                            [ ticTacToeBoard |> .bottomMiddle |> boardSquareValue |> Html.text ]
                        ]
                    ]
                , Html.td
                    [ style "border" "1px solid #000"
                    , style "border-collapse" "collapse"
                    ]
                    [ Html.button
                        [ style "width" "100px"
                        , style "height" "100px"
                        , style "background" "none"
                        , style "border" "none"
                        , onClick ClickedBottomRight
                        ]
                        [ Html.span
                            [ style "font-size" "30pt" ]
                            [ ticTacToeBoard |> .bottomRight |> boardSquareValue |> Html.text ]
                        ]
                    ]
                ]
            ]
        , Html.text ("Winner: " ++ Debug.toString (winner ticTacToeBoard))
        , Html.button [ onClick ClickedReset ] [ Html.text "Reset" ]
        ]
