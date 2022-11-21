module TicTacToe3 exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (disabled, style, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra



-- MAIN


main : Program () Board BrowserInteraction
main =
    Browser.sandbox
        { init = initialBoard
        , update = updateBoard
        , view = boardHtml
        }



-- MODEL


type BoardSquareValue
    = X
    | O
    | Empty


type alias Board =
    Array (Array BoardSquareValue)


emptyBoard : Int -> Board
emptyBoard boardSize =
    Array.repeat boardSize (Array.repeat boardSize Empty)


initialBoardSize : Int
initialBoardSize =
    4


initialBoard : Board
initialBoard =
    emptyBoard initialBoardSize



-- UPDATE


type alias BoardSquarePosition =
    ( Int, Int )


type BrowserInteraction
    = Clicked BoardSquarePosition
    | ClickedReset
    | ClickedUpdateBoardSize String


updateBoard : BrowserInteraction -> Board -> Board
updateBoard browserInteraction board =
    case browserInteraction of
        Clicked ( row, col ) ->
            if winner board == Empty then
                case Array.get row board of
                    Just boardRow ->
                        case Array.get col boardRow of
                            Just boardSquareValue ->
                                if boardSquareValue == Empty then
                                    Array.set row (Array.set col (nextTurn board) boardRow) board

                                else
                                    board

                            Nothing ->
                                board

                    Nothing ->
                        board

            else
                board

        ClickedReset ->
            emptyBoard (Array.length board)

        ClickedUpdateBoardSize boardSize ->
            case String.toInt boardSize of
                Just boardSizeAsInt ->
                    emptyBoard boardSizeAsInt

                Nothing ->
                    emptyBoard 0


boardAsLists : Board -> List (List BoardSquareValue)
boardAsLists board =
    board
        |> Array.toList
        |> List.map (\row -> Array.toList row)


nextTurn : Board -> BoardSquareValue
nextTurn board =
    let
        pastTurns =
            List.concat (boardAsLists board)

        numberOfXsTurns =
            pastTurns
                |> List.filter ((==) X)
                |> List.length

        numberOfOsTurns =
            pastTurns
                |> List.filter ((==) O)
                |> List.length

        xsTurn =
            numberOfXsTurns <= numberOfOsTurns
    in
    if xsTurn then
        X

    else
        O


winner : Board -> BoardSquareValue
winner board =
    let
        currentBoardSize =
            Array.length board

        listBoard =
            boardAsLists board

        wonARow boardSquareValue =
            listBoard
                |> List.any (List.all ((==) boardSquareValue))

        wonAColumn boardSquareValue =
            listBoard
                |> List.Extra.transpose
                |> List.any (List.all ((==) boardSquareValue))

        wonBackslash boardSquareValue =
            listBoard
                |> List.concat
                |> List.indexedMap Tuple.pair
                |> List.filter
                    (\listItemWithIndex ->
                        remainderBy (currentBoardSize + 1) (Tuple.first listItemWithIndex) == 0
                    )
                |> List.map Tuple.second
                |> List.all ((==) boardSquareValue)

        wonForwardSlash boardSquareValue =
            listBoard
                |> List.concat
                |> List.indexedMap Tuple.pair
                |> List.filter
                    (\listItemWithIndex ->
                        remainderBy (currentBoardSize - 1) (Tuple.first listItemWithIndex)
                            == 0
                            && (Tuple.first listItemWithIndex /= 0)
                            && (Tuple.first listItemWithIndex /= currentBoardSize ^ 2 - 1)
                    )
                |> List.map Tuple.second
                |> List.all ((==) boardSquareValue)

        won boardSquareValue =
            wonARow boardSquareValue
                || wonAColumn boardSquareValue
                || wonBackslash boardSquareValue
                || wonForwardSlash boardSquareValue
    in
    if X |> won then
        X

    else if O |> won then
        O

    else
        Empty



-- VIEW


boardSquareValueToString : BoardSquareValue -> String
boardSquareValueToString boardSquareValue =
    case boardSquareValue of
        X ->
            "X"

        O ->
            "O"

        Empty ->
            ""


boardSquareHtml : Int -> Int -> BoardSquareValue -> Html BrowserInteraction
boardSquareHtml boardRowIndex boardColumnIndex boardSquareValue =
    Html.td
        [ style "border" "1px solid #000"
        , style "border-collapse" "collapse"
        ]
        [ Html.button
            [ style "width" "100px"
            , style "height" "100px"
            , style "background" "none"
            , style "border" "none"
            , onClick (Clicked ( boardRowIndex, boardColumnIndex ))
            ]
            [ Html.span
                [ style "font-size" "30pt" ]
                [ boardSquareValue |> boardSquareValueToString |> Html.text ]
            ]
        ]


boardHtml : Board -> Html BrowserInteraction
boardHtml board =
    Html.div []
        [ Html.text "Board Size"
        , Html.input
            [ type_ "text"
            , value (String.fromInt (Array.length board))
            , disabled (not (boardIsEmpty board))
            , onInput ClickedUpdateBoardSize
            ]
            []
        , Html.table
            [ style "border" "1px solid #000", style "border-collapse" "collapse" ]
            (Array.toList
                (Array.indexedMap
                    (\boardRowIndex boardRow ->
                        Html.tr []
                            (Array.toList
                                (Array.indexedMap
                                    (\boardColumnIndex boardSquareValue ->
                                        boardSquareHtml boardRowIndex boardColumnIndex boardSquareValue
                                    )
                                    boardRow
                                )
                            )
                    )
                    board
                )
            )
        , Html.text ("Winner: " ++ viewWinner (winner board))
        , newGameButton board
        ]


newGameButton : Board -> Html BrowserInteraction
newGameButton board =
    if boardIsEmpty board then
        Html.text ""

    else
        Html.button [ onClick ClickedReset ] [ Html.text "New Game" ]


viewWinner : BoardSquareValue -> String
viewWinner boardSquareValue =
    case boardSquareValue of
        X ->
            "X"

        O ->
            "O"

        Empty ->
            "None"


boardIsEmpty : Board -> Bool
boardIsEmpty board =
    let
        boardList =
            List.map Array.toList (Array.toList board)
    in
    List.all ((==) True) (List.map (\row -> List.all ((==) Empty) row) boardList)
