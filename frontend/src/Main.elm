module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Comment exposing (Comment(..), Preview(..))
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Maybe exposing (Maybe(..))
import Slug
import Url exposing (Url)
import Url.Builder
import Url.Parser
import Url.Parser.Query


type alias Model =
    { search : String
    , response : Maybe (List ( Comment Preview, Float ))
    , session : Navigation.Key
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { response = Nothing, search = "", session = key }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse (Ok response) ->
            ( { model | response = Just response }, Cmd.none )

        GotResponse (Err error) ->
            let
                _ =
                    Debug.log "error" error
            in
            ( { model | response = Just [] }, Cmd.none )

        SubmitForm ->
            --  (\xys -> GotResponse (Result.map (List.map (\( y, x ) -> ( x, y ))) xys)) )k
            ( model, Comment.listWithSearch model.search GotResponse )

        SetSearch search ->
            ( model, Navigation.pushUrl model.session (Url.Builder.relative [] [ Url.Builder.string "q" search ]) )

        UrlRequested _ ->
            ( model, Cmd.none )

        UrlChanged url ->
            let
                maybeSearch =
                    { url | path = "" }
                        |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string "q"))
            in
            case maybeSearch of
                Just (Just search) ->
                    ( { model | search = search }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


type Msg
    = SubmitForm
    | SetSearch String
    | GotResponse (Result Http.Error (List ( Comment Preview, Float )))
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url



-- get : String -> Cmd Msg
-- get search =
--     Http.get
--         -- Http.re
--         { url =
--             Url.Builder.crossOrigin
--                 "http://localhost:3000"
--                 []
--                 [ Url.Builder.string "q" search ]
--         , expect =
--             Http.expectString GotResponse
--         }


view : Model -> Browser.Document Msg
view model =
    let
        responseString =
            case model.response of
                Just response ->
                    response
                        |> List.map
                            (\( Comment { slug } Preview, score ) ->
                                Slug.toString slug ++ ": " ++ String.fromFloat score
                            )
                        |> String.join "\n"

                Nothing ->
                    ""
    in
    Browser.Document
        "Oru"
        [ Html.div []
            [ Html.form [ Events.onSubmit SubmitForm ]
                [ Html.input
                    [ Attributes.type_ "search"
                    , Attributes.value model.search
                    , Events.onInput SetSearch
                    ]
                    []
                , Html.button
                    [ Attributes.class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                    ]
                    [ Html.text "Search" ]
                ]
            , Html.pre [] [ Html.text responseString ]
            ]
        ]
