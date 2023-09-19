module Api.Data exposing
    ( Comment
    , commentDecoder
    , commentEncoder
    )

import Json.Decode
import Json.Encode


commentDecoder : Json.Decode.Decoder Comment
commentDecoder =
    Json.Decode.succeed
        (\a b c d ->
            { title = a
            , slug = b
            , rating = c
            , body = d
            }
        )
        |> Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "title" Json.Decode.string))
        |> Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "slug" Json.Decode.string))
        |> Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "rating" (Json.Decode.nullable Json.Decode.int)))
        |> Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "body" Json.Decode.string))


commentEncoder : Comment -> Json.Encode.Value
commentEncoder a =
    Json.Encode.object
        [ ( "title", Json.Encode.string a.title )
        , ( "slug", Json.Encode.string a.slug )
        , ( "rating", (\b -> Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int b)) a.rating )
        , ( "body", Json.Encode.string a.body )
        ]


type alias Comment =
    { title : String, slug : String, rating : Maybe Int, body : String }
