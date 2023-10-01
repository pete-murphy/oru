module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Comment exposing (Comment(..), Preview(..))
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Icon
import Maybe exposing (Maybe(..))
import Url exposing (Url)
import Url.Builder
import Url.Parser
import Url.Parser.Query


type alias Model =
    { session : Navigation.Key
    , search : String
    , response : Maybe (List ( Comment Preview, Float ))
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


maybeSearchFromUrl : Url -> Maybe String
maybeSearchFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string "q"))
        |> Maybe.andThen (\maybeSearch -> maybeSearch)


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        maybeSearch =
            maybeSearchFromUrl url
    in
    ( { session = key
      , search = Maybe.withDefault "" maybeSearch
      , response = Nothing
      }
    , maybeSearch
        |> Maybe.map (\search -> Comment.listWithSearch search GotResponse)
        |> Maybe.withDefault Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse (Ok response) ->
            ( { model | response = Just response }
            , Cmd.none
            )

        GotResponse (Err error) ->
            let
                _ =
                    Debug.toString error
            in
            ( { model | response = Just [] }
            , Cmd.none
            )

        FormSubmitted ->
            ( model
            , Comment.listWithSearch model.search GotResponse
            )

        SearchChanged search ->
            ( model
            , Navigation.replaceUrl
                model.session
                (Url.Builder.relative
                    []
                    [ Url.Builder.string "q" search ]
                )
            )

        UrlRequested _ ->
            ( model
            , Cmd.none
            )

        UrlChanged url ->
            case maybeSearchFromUrl url of
                Just search ->
                    ( { model | search = search }
                    , Comment.listWithSearch search GotResponse
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


type Msg
    = FormSubmitted
    | SearchChanged String
    | GotResponse (Result Http.Error (List ( Comment Preview, Float )))
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Oru"
        [ Html.main_
            [ Attributes.class "flex flex-col"
            ]
            [ Html.div
                [ Attributes.class "h-[max(0rem,calc(25vh-4rem))]"
                ]
                []
            , Html.div
                [ Attributes.class "sticky top-0 bg-white p-4 shadow-sm" ]
                [ Html.form
                    [ Attributes.class "flex gap-4"
                    , Events.onSubmit FormSubmitted
                    ]
                    [ Html.div
                        [ Attributes.class "[&:has(:focus-visible)]:outline-blue-500 outline-none transition-all border border-gray-300 rounded-lg flex gap-2 items-center px-4" ]
                        [ Icon.search
                        , Html.input
                            [ Attributes.class "py-2 w-[20ch] outline-none bg-transparent"
                            , Attributes.type_ "search"
                            , Attributes.placeholder "Search"
                            , Attributes.attribute "defaultValue" model.search
                            , Events.onInput SearchChanged
                            ]
                            []
                        ]
                    ]
                ]
            , Html.div
                [ Attributes.class "p-4 bg-gray-100" ]
                (model.response
                    |> Maybe.map (\res -> [ listResponse res ])
                    |> Maybe.withDefault []
                )
            ]
        ]


listResponse : List ( Comment Preview, Float ) -> Html.Html Msg
listResponse response =
    Html.ul []
        (response
            |> List.map
                (\( Comment { title, movieTitle, rating } Preview, score ) ->
                    Html.li
                        [ Attributes.class "mb-2" ]
                        [ Html.div
                            [ Attributes.class "font-bold flex gap-2" ]
                            [ Html.span
                                [ Attributes.class "text-gray-400" ]
                                [ Html.text movieTitle ]
                            , Html.span
                                [ Attributes.class "text-gray-700" ]
                                [ Html.text title ]
                            ]
                        , Html.div
                            [ Attributes.class "flex gap-2 text-gray-500" ]
                            [ Html.span
                                []
                                [ Html.text (Maybe.withDefault "N/A" (Maybe.map String.fromInt rating)) ]
                            , Html.span
                                []
                                [ Html.text (String.fromFloat score) ]
                            ]
                        ]
                )
        )
