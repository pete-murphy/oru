module Comment exposing
    ( Comment(..)
    , Full(..)
    , Preview(..)
    , bodyToHtml
    , decoder
    , decoderPreview
    , encode
    , encodePreview
    , get
    , list
    , listWithSearch
    )

import Api
import Api.Data
import Api.Endpoint as Endpoint
import CommentId exposing (CommentId)
import Html exposing (Attribute, Html)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown
import Slug exposing (Slug)
import Tuple
import Url.Builder


type Comment a
    = Comment Internals a


type alias Internals =
    { title : String
    , movieTitle : String
    , id : CommentId
    , slug : Slug
    , rating : Maybe Int
    }


type Preview
    = Preview


type Full
    = Full Body


type Body
    = Body String


encode : Comment Full -> Encode.Value
encode (Comment { title, movieTitle, slug, rating } (Full (Body body))) =
    Api.Data.fullCommentEncoder
        { title = title
        , movieTitle = movieTitle
        , slug = Slug.toString slug
        , rating = rating
        , body = body
        }


encodePreview : Comment Preview -> Encode.Value
encodePreview (Comment { title, movieTitle, slug, rating } Preview) =
    Api.Data.previewCommentEncoder
        { title = title
        , movieTitle = movieTitle
        , slug = Slug.toString slug
        , rating = rating
        }


decoder : Decode.Decoder (Comment Full)
decoder =
    Api.Data.fullCommentDecoder
        |> Decode.andThen
            (\{ title, movieTitle, rating, body } ->
                Decode.field "slug" Slug.decoder
                    |> Decode.map
                        (\slug ->
                            { title = title
                            , movieTitle = movieTitle
                            , slug = slug
                            , rating = rating
                            , body = body
                            }
                        )
            )
        |> Decode.andThen
            (\{ title, movieTitle, slug, rating, body } ->
                -- TODO: Don't use the slug as the id
                Decode.field "slug" CommentId.decoder
                    |> Decode.map
                        (\id ->
                            { title = title
                            , movieTitle = movieTitle
                            , id = id
                            , slug = slug
                            , rating = rating
                            , body = body
                            }
                        )
            )
        |> Decode.map
            (\{ title, movieTitle, id, slug, rating, body } ->
                Comment
                    { title = title
                    , movieTitle = movieTitle
                    , id = id
                    , slug = slug
                    , rating = rating
                    }
                    (Full (Body body))
            )


decoderPreview : Decode.Decoder (Comment Preview)
decoderPreview =
    Api.Data.previewCommentDecoder
        |> Decode.andThen
            (\{ title, movieTitle, rating } ->
                Decode.field "slug" Slug.decoder
                    |> Decode.map
                        (\slug ->
                            { title = title
                            , movieTitle = movieTitle
                            , slug = slug
                            , rating = rating
                            }
                        )
            )
        |> Decode.andThen
            (\{ title, movieTitle, slug, rating } ->
                -- TODO: Don't use the slug as the id
                Decode.field "slug" CommentId.decoder
                    |> Decode.map
                        (\id ->
                            { title = title
                            , movieTitle = movieTitle
                            , id = id
                            , slug = slug
                            , rating = rating
                            }
                        )
            )
        |> Decode.map
            (\internals ->
                Comment internals Preview
            )


bodyToHtml : List (Attribute msg) -> Body -> Html msg
bodyToHtml attributes (Body markdownString) =
    Markdown.toHtml attributes markdownString



-- API Requests


get : CommentId -> (Result Http.Error (Comment Full) -> msg) -> Cmd msg
get id toMsg =
    Api.get (Endpoint.comment id) toMsg decoder


list : (Result Http.Error (List ( Comment Preview, Float )) -> msg) -> Cmd msg
list toMsg =
    let
        decoder_ =
            Decode.map2 Tuple.pair
                (Decode.index 0 decoderPreview)
                (Decode.index 1 Decode.float)
    in
    Api.get (Endpoint.comments []) toMsg (Decode.list decoder_)


listWithSearch : String -> (Result Http.Error (List ( Comment Preview, Float )) -> msg) -> Cmd msg
listWithSearch search toMsg =
    let
        decoder_ =
            Decode.map2 Tuple.pair
                (Decode.index 0 decoderPreview)
                (Decode.index 1 Decode.float)
    in
    Api.getWithTracker
        (Just search)
        (Endpoint.comments
            [ Url.Builder.string "q" search
            ]
        )
        toMsg
        (Decode.list decoder_)
