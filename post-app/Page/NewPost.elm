module Page.NewPost exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Post exposing (Post, newPostEncoder, postDecoder)
import Route exposing (Route)


type alias Model =
    { navKey : Nav.Key
    , title : String
    , authorName : String
    , authorUrl : String
    , createError : Maybe String
    }


type Msg
    = CreatePost
    | UpdateTitle String
    | UpdateAuthorName String
    | UpdateAuthorUrl String
    | CreatedPost (Result Http.Error Post)


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( { navKey = navKey
      , title = ""
      , authorName = ""
      , authorUrl = ""
      , createError = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreatePost ->
            ( model, createPost model.title model.authorName model.authorUrl )

        UpdateTitle title ->
            ( { model | title = title }, Cmd.none )

        UpdateAuthorName authorName ->
            ( { model | authorName = authorName }, Cmd.none )

        UpdateAuthorUrl authorUrl ->
            ( { model | authorUrl = authorUrl }, Cmd.none )

        CreatedPost (Ok post) ->
            ( model, Route.pushUrl Route.Posts model.navKey )

        CreatedPost (Err error) ->
            ( { model | createError = Just (buildErrorMessage error) }, Cmd.none )


createPost : String -> String -> String -> Cmd Msg
createPost title authorName authorUrl =
    Http.post
        { url = "http://localhost:5019/posts"
        , body = Http.jsonBody (newPostEncoder title authorName authorUrl)
        , expect = Http.expectJson CreatedPost postDecoder
        }


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "New Post" ]
        , Html.form []
            [ div []
                [ text "Title"
                , br [] []
                , input
                    [ type_ "text"
                    , value model.title
                    , onInput UpdateTitle
                    ]
                    []
                ]
            , br [] []
            , div []
                [ text "Author Name"
                , br [] []
                , input
                    [ type_ "text"
                    , value model.authorName
                    , onInput UpdateAuthorName
                    ]
                    []
                ]
            , br [] []
            , div []
                [ text "Author URL"
                , br [] []
                , input
                    [ type_ "text"
                    , value model.authorUrl
                    , onInput UpdateAuthorUrl
                    ]
                    []
                ]
            , br [] []
            , div []
                [ button [ type_ "button", onClick CreatePost ]
                    [ text "Submit " ]
                ]
            ]
        , viewCreateErrorMessage model.createError
        ]


viewCreateErrorMessage : Maybe String -> Html Msg
viewCreateErrorMessage maybeCreateErrorMessage =
    case maybeCreateErrorMessage of
        Just createErrorMessage ->
            div []
                [ h3 [] [ text "Couldn't create post" ]
                , text ("Error: " ++ createErrorMessage)
                ]

        Nothing ->
            text ""
