module Api exposing
    ( decodeErrors
    , get
    )

{-| This module is responsible for communicating to the API.

It exposes an opaque Endpoint type which is guaranteed to point to the correct URL.

-}

import Api.Endpoint as Endpoint exposing (Endpoint)
import Http
import Json.Decode exposing (Decoder)
import Url exposing (Protocol(..))


get : Endpoint -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
get url toMsg decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Many API endpoints include an "errors" field in their BadStatus responses.
-}
decodeErrors : Http.Error -> List String
decodeErrors error =
    case error of
        Http.BadStatus response ->
            [ "BadStatus: " ++ String.fromInt response ]

        _ ->
            [ "Server error" ]
