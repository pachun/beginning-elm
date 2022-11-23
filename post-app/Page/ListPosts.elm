module Page.ListPosts exposing (Model, Msg, init, update, view)

import Browser
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Post exposing (Post, PostId, postsDecoder)
import RemoteData exposing (RemoteData, WebData)


type alias Model =
    { posts : WebData (List Post), deleteError : Maybe String }


type Msg
    = GetPosts
    | GotPosts (WebData (List Post))
    | DeletePost PostId
    | PostDeleted (Result Http.Error String)


init : ( Model, Cmd Msg )
init =
    ( { deleteError = Nothing, posts = RemoteData.Loading }, getPosts )


getPosts : Cmd Msg
getPosts =
    Http.get
        { url = "http://localhost:5019/posts"
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> GotPosts)
                postsDecoder
        }


deletePost : PostId -> Cmd Msg
deletePost postId =
    Http.request
        { body = Http.emptyBody
        , method = "DELETE"
        , headers = []
        , url = "http://localhost:5019/posts/" ++ Post.idToString postId
        , expect = Http.expectString PostDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPosts ->
            ( { model | posts = RemoteData.Loading }, getPosts )

        GotPosts response ->
            ( { model | posts = response }, Cmd.none )

        DeletePost postId ->
            ( model, deletePost postId )

        PostDeleted (Ok _) ->
            ( { model | posts = RemoteData.Loading }, getPosts )

        PostDeleted (Err error) ->
            ( { model | deleteError = Just (buildErrorMessage error) }, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
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
                , br [] []
                , br [] []
                , a [ href "/posts/new" ] [ text "Create New Post" ]
                , h1 [] [ text "Posts" ]
                , viewPosts posts
                , viewDeleteError model.deleteError
                ]


viewPosts : List Post -> Html Msg
viewPosts posts =
    table []
        ([ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Title" ]
            , th [] [ text "Author" ]
            ]
         ]
            ++ List.map tablePost posts
        )


viewDeleteError : Maybe String -> Html Msg
viewDeleteError maybeDeleteError =
    case maybeDeleteError of
        Just deleteError ->
            div []
                [ h3 [] [ text "Couldn't delete post at this time." ]
                , text ("Error: " ++ deleteError)
                ]

        Nothing ->
            text ""


tablePost : Post -> Html Msg
tablePost post =
    tr []
        [ td [] [ text (Post.idToString post.id) ]
        , td [] [ text post.title ]
        , td [] [ a [ href post.authorUrl ] [ text post.authorName ] ]
        , td [] [ a [ href ("/posts/" ++ Post.idToString post.id) ] [ text "Edit" ] ]
        , td [] [ button [ onClick (DeletePost post.id) ] [ text "Delete" ] ]
        ]
