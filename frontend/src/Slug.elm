module Slug exposing
    ( Slug
    , decoder
    , decoderFromString
    , toString
    )

import Json.Decode exposing (Decoder)


type Slug
    = Slug String


toString : Slug -> String
toString (Slug slug) =
    slug


decoder : Decoder Slug
decoder =
    Json.Decode.string
        |> Json.Decode.map Slug


decoderFromString : Decoder String -> Decoder Slug
decoderFromString =
    Json.Decode.map Slug
