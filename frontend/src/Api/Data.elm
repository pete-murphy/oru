module Api.Data exposing
    ( fullCommentDecoder
    , fullCommentEncoder
    , previewCommentDecoder
    , previewCommentEncoder
    , FullComment
    , PreviewComment
    )

import Json.Decode
import Json.Encode


fullCommentDecoder : Json.Decode.Decoder FullComment
fullCommentDecoder =
    Json.Decode.succeed (\a b c d e -> { title = a
    , movieTitle = b
    , slug = c
    , rating = d
    , body = e }) |>
    Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "title" Json.Decode.string)) |>
    Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "movieTitle" Json.Decode.string)) |>
    Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "slug" Json.Decode.string)) |>
    Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "rating" (Json.Decode.nullable Json.Decode.int))) |>
    Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "body" Json.Decode.string))


fullCommentEncoder : FullComment -> Json.Encode.Value
fullCommentEncoder a =
    Json.Encode.object [ ("title" , Json.Encode.string a.title)
    , ("movieTitle" , Json.Encode.string a.movieTitle)
    , ("slug" , Json.Encode.string a.slug)
    , ("rating" , (\b -> Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int b)) a.rating)
    , ("body" , Json.Encode.string a.body) ]


previewCommentDecoder : Json.Decode.Decoder PreviewComment
previewCommentDecoder =
    Json.Decode.succeed (\a b c d -> { title = a
    , movieTitle = b
    , slug = c
    , rating = d }) |>
    Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "title" Json.Decode.string)) |>
    Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "movieTitle" Json.Decode.string)) |>
    Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "slug" Json.Decode.string)) |>
    Json.Decode.andThen (\a -> Json.Decode.map a (Json.Decode.field "rating" (Json.Decode.nullable Json.Decode.int)))


previewCommentEncoder : PreviewComment -> Json.Encode.Value
previewCommentEncoder a =
    Json.Encode.object [ ("title" , Json.Encode.string a.title)
    , ("movieTitle" , Json.Encode.string a.movieTitle)
    , ("slug" , Json.Encode.string a.slug)
    , ("rating" , (\b -> Maybe.withDefault Json.Encode.null (Maybe.map Json.Encode.int b)) a.rating) ]


type alias FullComment  =
    { title : String
    , movieTitle : String
    , slug : String
    , rating : Maybe Int
    , body : String }


type alias PreviewComment  =
    { title : String, movieTitle : String, slug : String, rating : Maybe Int }
