module Page.EditPost exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Post exposing (Post, PostId, postDecoder, postEncoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)


type alias Model =
    { navKey : Nav.Key
    , post : WebData Post
    , saveError : Maybe String
    }


init : PostId -> Nav.Key -> ( Model, Cmd Msg )
init postId navKey =
    ( initialModel navKey, getPost postId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey, post = RemoteData.Loading, saveError = Nothing }


getPost : PostId -> Cmd Msg
getPost postId =
    Http.get
        { url = "http://localhost:5019/posts/" ++ Post.idToString postId
        , expect =
            postDecoder
                |> Http.expectJson (RemoteData.fromResult >> PostReceived)
        }


type Msg
    = PostReceived (WebData Post)
    | UpdateTitle String
    | UpdateAuthorName String
    | UpdateAuthorUrl String
    | SavePost
    | PostSaved (Result Http.Error Post)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostReceived post ->
            ( { model | post = post }, Cmd.none )

        UpdateTitle title ->
            ( { model
                | post =
                    RemoteData.map
                        (\postData -> { postData | title = title })
                        model.post
              }
            , Cmd.none
            )

        UpdateAuthorName authorName ->
            ( { model
                | post =
                    RemoteData.map
                        (\postData -> { postData | authorName = authorName })
                        model.post
              }
            , Cmd.none
            )

        UpdateAuthorUrl authorUrl ->
            ( { model
                | post =
                    RemoteData.map
                        (\postData -> { postData | authorUrl = authorUrl })
                        model.post
              }
            , Cmd.none
            )

        SavePost ->
            ( model, savePost model.post )

        PostSaved (Ok postData) ->
            let
                post =
                    RemoteData.succeed postData
            in
            ( { model | post = post }, Route.pushUrl Route.Posts model.navKey )

        PostSaved (Err error) ->
            ( { model | saveError = Just (buildErrorMessage error) }, Cmd.none )


savePost : WebData Post -> Cmd Msg
savePost post =
    case post of
        RemoteData.Success postData ->
            Http.request
                { method = "PATCH"
                , headers = []
                , url = "http://localhost:5019/posts/" ++ Post.idToString postData.id
                , body = Http.jsonBody (postEncoder postData)
                , expect = Http.expectJson PostSaved postDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit Post" ]
        , viewPost model.post
        , viewSaveError model.saveError
        ]


viewPost : WebData Post -> Html Msg
viewPost post =
    case post of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Post..." ]

        RemoteData.Success postData ->
            editForm postData

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


editForm : Post -> Html Msg
editForm post =
    Html.form []
        [ div []
            [ text "Title"
            , br [] []
            , input
                [ type_ "text"
                , value post.title
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
                , value post.authorName
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
                , value post.authorUrl
                , onInput UpdateAuthorUrl
                ]
                []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick SavePost ]
                [ text "Submit " ]
            ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch post at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewSaveError : Maybe String -> Html msg
viewSaveError maybeSaveError =
    case maybeSaveError of
        Just saveError ->
            div [] [ h3 [] [ text "Couldn't save post at this time." ], text ("Error: " ++ saveError) ]

        Nothing ->
            text ""
