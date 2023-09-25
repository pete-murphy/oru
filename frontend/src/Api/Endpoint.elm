module Api.Endpoint exposing
    ( Endpoint
    , comment
    , comments
    , request
    )

import CommentId exposing (CommentId)
import Http
import Url.Builder exposing (QueryParameter)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect msg
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , tracker : Maybe String
    }
    -> Cmd msg
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , tracker = config.tracker
        }


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin "http://localhost:3000"
        -- ("api" :: paths)
        paths
        queryParams
        |> Endpoint



-- ENDPOINTS


comments : List QueryParameter -> Endpoint
comments =
    url [ "comments" ]


comment : CommentId -> Endpoint
comment commentId =
    url [ "comments", CommentId.toString commentId ] []
