module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Maybe exposing (Maybe(..))
import Url.Builder


type alias Model =
    { search : String
    , response : Maybe String
    }


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
    ( { response = Nothing, search = "" }, Cmd.none )


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
            ( { model | search = search }, get search )


type Msg
    = SubmitForm
    | SetSearch String
    | Response (Result Http.Error String)


get : String -> Cmd Msg
get search =
    Http.get
        { url = Url.Builder.crossOrigin "http://localhost:3000" [] [ Url.Builder.string "q" search ]
        , expect = Http.expectString Response
        }



-- get : String -> Cmd Msg
-- get search =
--     Http.get
--         { url = "http://localhost:3000?q=" ++ search
--         , expect = Http.expectString Response
--         }
-- formUrlencoded : List ( String, String ) -> String
-- formUrlencoded object =
--     object
--         |> List.map
--             (\( name, value ) ->
--                 Http.encodeUri name
--                     ++ "="
--                     ++ Http.encodeUri value
--             )
--         |> String.join "&"


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.form [ Events.onSubmit SubmitForm ]
            [ Html.input
                [ Attributes.value model.search
                , Events.onInput SetSearch
                ]
                []
            , Html.button [] [ Html.text "Search" ]
            , Html.pre [] [ Html.text (Maybe.withDefault "" model.response) ]
            ]
        ]
