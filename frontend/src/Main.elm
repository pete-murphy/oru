module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Events
import Http
import Maybe exposing (Maybe(..))


type alias Model =
    Maybe String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotBook (Ok book) ->
            ( Just book, Cmd.none )

        GotBook (Err _) ->
            ( Nothing, Cmd.none )

        GotItems (Ok items) ->
            ( List.head items, Cmd.none )

        GotItems (Err _) ->
            ( Nothing, Cmd.none )

        GetBook ->
            ( Nothing, getBook )


type Msg
    = GotBook (Result Http.Error String)
    | GotItems (Result Http.Error (List String))
    | GetBook


getBook : Cmd Msg
getBook =
    Http.get
        { url = "https://elm-lang.org/assets/public-opinion.txt"
        , expect = Http.expectString GotBook
        }


view : Model -> Html Msg
view model =
    let
        text =
            Maybe.withDefault "No book" model
    in
    Html.div []
        [ Html.button [ Events.onClick GetBook ] [ Html.text "Get book" ]
        , Html.div [] [ Html.text text ]
        ]
