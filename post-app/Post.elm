module Post exposing (Post, PostId, idParser, idToString, postDecoder, postEncoder, postsDecoder)

import Html exposing (..)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)


type PostId
    = PostId Int


type alias Post =
    { id : PostId
    , title : String
    , authorName : String
    , authorUrl : String
    }


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" (Decode.map PostId int)
        |> required "title" string
        |> required "authorName" string
        |> required "authorUrl" string


postsDecoder : Decoder (List Post)
postsDecoder =
    list postDecoder


idToString : PostId -> String
idToString (PostId postId) =
    String.fromInt postId


idParser : Parser (PostId -> a) a
idParser =
    custom "Post ID" <|
        \postId ->
            case String.toInt postId of
                Just postIdAsInt ->
                    Just (PostId postIdAsInt)

                Nothing ->
                    Nothing


postEncoder : Post -> Encode.Value
postEncoder post =
    Encode.object
        [ ( "id", encodeId post.id )
        , ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]


encodeId : PostId -> Encode.Value
encodeId (PostId postId) =
    Encode.int postId
