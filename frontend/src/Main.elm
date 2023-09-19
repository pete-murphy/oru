module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Maybe exposing (Maybe(..))
import Url exposing (Url)
import Url.Builder


type alias Model =
    { search : String
    , response : Maybe String
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
        Response (Ok response) ->
            ( { model | response = Just response }, Cmd.none )

        Response (Err _) ->
            ( { model | response = Just "Bad" }, Cmd.none )

        SubmitForm ->
            ( model, get model.search )

        SetSearch search ->
            ( model, Navigation.pushUrl model.session (Url.Builder.relative [] [ Url.Builder.string "q" search ]) )

        UrlRequested _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )


type Msg
    = SubmitForm
    | SetSearch String
    | Response (Result Http.Error String)
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url


get : String -> Cmd Msg
get search =
    Http.get
        { url =
            Url.Builder.crossOrigin
                "http://localhost:3000"
                []
                [ Url.Builder.string "q" search ]
        , expect =
            Http.expectString Response
        }


view : Model -> Browser.Document Msg
view model =
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
                , Html.button [] [ Html.text "Search" ]
                ]
            , Html.pre [] [ Html.text (Maybe.withDefault "" model.response) ]
            ]
        ]
