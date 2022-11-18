module DecodingJson exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData, WebData)


type alias Post =
    { id : Int
    , title : String
    , authorName : String
    , authorUrl : String
    }


type alias Model =
    { posts : WebData (List Post) }


view : Model -> Html Msg
view model =
    div []
        [ postsOrErrorMessage model
        ]


postsOrErrorMessage : Model -> Html Msg
postsOrErrorMessage model =
    case model.posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Failure error ->
            div []
                [ h3 [] [ text "Couldn't Fetch Posts" ]
                , text (buildErrorMessage error)
                ]

        RemoteData.Success posts ->
            div []
                [ button [ onClick GetPosts ] [ text "Refresh Posts" ]
                , h1 [] [ text "Posts" ]
                , table []
                    ([ tr []
                        [ th [] [ text "ID" ]
                        , th [] [ text "Title" ]
                        , th [] [ text "Author" ]
                        ]
                     ]
                        ++ List.map tablePost posts
                    )
                ]


tablePost : Post -> Html Msg
tablePost post =
    tr []
        [ td [] [ text (String.fromInt post.id) ]
        , td [] [ text post.title ]
        , td [] [ a [ href post.authorUrl ] [ text post.authorName ] ]
        ]


type Msg
    = GetPosts
    | GotPosts (WebData (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPosts ->
            ( { model | posts = RemoteData.Loading }, getPosts )

        GotPosts respose ->
            ( { model | posts = respose }, Cmd.none )


getPosts : Cmd Msg
getPosts =
    Http.get
        { url = "http://localhost:5019/posts"
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> GotPosts)
                (list postDecoder)
        }


postDecoder : Decode.Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" int
        |> required "title" string
        |> required "authorName" string
        |> required "authorUrl" string


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
    ( { posts = RemoteData.Loading }, getPosts )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
