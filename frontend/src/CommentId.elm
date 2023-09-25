module CommentId exposing
    ( CommentId
    , decoder
    , toString
    )

import Json.Decode as Decode exposing (Decoder)



-- TYPES


type CommentId
    = CommentId String



-- CREATE


decoder : Decoder CommentId
decoder =
    Decode.map CommentId Decode.string



-- TRANSFORM


toString : CommentId -> String
toString (CommentId id) =
    id
