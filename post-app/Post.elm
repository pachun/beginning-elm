module Post exposing (Post, PostId, idToString, postDecoder, postsDecoder)

import Html exposing (..)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


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
