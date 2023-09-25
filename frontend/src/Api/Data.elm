module Api.Data exposing
    ( FullComment
    , PreviewComment
    , fullCommentDecoder
    , fullCommentEncoder
    , previewCommentDecoder
    , previewCommentEncoder
    )

import Json.Decode
import Json.Encode


fullCommentDecoder : Json.Decode.Decoder FullComment
fullCommentDecoder =
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


fullCommentEncoder : FullComment -> Json.Encode.Value
fullCommentEncoder a =
    Json.Encode.object
        [ ( "title", Json.Encode.string a.title )
        , ( "slug", Json.Encode.string a.slug )
        , ( "rating", (\b -> Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int b)) a.rating )
        , ( "body", Json.Encode.string a.body )
        ]


previewCommentDecoder : Json.Decode.Decoder PreviewComment
previewCommentDecoder =
    Json.Decode.succeed (\a b c -> { title = a, slug = b, rating = c })
        |> Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "title" Json.Decode.string))
        |> Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "slug" Json.Decode.string))
        |> Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "rating" (Json.Decode.nullable Json.Decode.int)))


previewCommentEncoder : PreviewComment -> Json.Encode.Value
previewCommentEncoder a =
    Json.Encode.object
        [ ( "title", Json.Encode.string a.title )
        , ( "slug", Json.Encode.string a.slug )
        , ( "rating", (\b -> Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int b)) a.rating )
        ]


type alias FullComment =
    { title : String, slug : String, rating : Maybe Int, body : String }


type alias PreviewComment =
    { title : String, slug : String, rating : Maybe Int }
