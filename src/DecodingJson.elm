module DecodingJson exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required, requiredAt)


type alias Post =
    { id : Int
    , title : String
    , authorName : String
    , authorUrl : String
    }


type alias Model =
    { posts : List Post
    , errorMessage : Maybe String
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ] [ text "Get Data From Server" ]
        , h1 [] [ text "Posts" ]
        , postsOrErrorMessage model
        ]


postsOrErrorMessage : Model -> Html Msg
postsOrErrorMessage model =
    case model.errorMessage of
        Just errorMessage ->
            div []
                [ h3 [] [ text "Couldn't Fetch Posts" ]
                , text errorMessage
                ]

        Nothing ->
            table []
                ([ tr []
                    [ th [] [ text "ID" ]
                    , th [] [ text "Title" ]
                    , th [] [ text "Author" ]
                    ]
                 ]
                    ++ List.map tablePost model.posts
                )


tablePost : Post -> Html Msg
tablePost post =
    tr []
        [ td [] [ text (String.fromInt post.id) ]
        , td [] [ text post.title ]
        , td [] [ a [ href post.authorUrl ] [ text post.authorName ] ]
        ]


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getPosts )

        DataReceived (Err error) ->
            ( { model | errorMessage = Just (buildErrorMessage error) }, Cmd.none )

        DataReceived (Ok posts) ->
            ( { model | posts = posts }, Cmd.none )


getPosts : Cmd Msg
getPosts =
    Http.get
        { url = "http://localhost:5019/posts"
        , expect = Http.expectJson DataReceived (list postDecoder)
        }


postDecoder : Decode.Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" int
        |> required "title" string
        |> requiredAt [ "author", "name" ] string
        |> requiredAt [ "author", "url" ] string


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = [], errorMessage = Nothing }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
