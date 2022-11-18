module HttpExamples exposing (Model)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http


type alias Model =
    { nicknames : List String
    , errorMessage : Maybe String
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ] [ text "Get data from server" ]
        , h3 [] [ text "Old School Main Characters" ]
        , ul [] (List.map viewNickname model.nicknames)
        ]


viewNickname : String -> Html Msg
viewNickname nickname =
    li [] [ text nickname ]


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error String)


url : String
url =
    "http://localhost:5016/old-school.txt"


getNicknames : Cmd Msg
getNicknames =
    Http.get
        { url = url
        , expect = Http.expectString DataReceived
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getNicknames )

        DataReceived (Ok nicknamesInOneString) ->
            let
                nicknames =
                    String.split "," nicknamesInOneString
            in
            ( { model | nicknames = nicknames }, Cmd.none )

        DataReceived (Err _) ->
            ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nicknames = [], errorMessage = Nothing }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
