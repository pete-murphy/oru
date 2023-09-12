module Main exposing (..)

import Browser
import Http
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D

main : Program () Int Msg
main =
  Browser.sandbox { init = 0, update = update, view = view }


type Msg
  = GotBook (Result Http.Error String)
  | GotItems (Result Http.Error (List String))

getBook : Cmd Msg
getBook =
  Http.get
    { url = "https://elm-lang.org/assets/public-opinion.txt"
    , expect = Http.expectString GotBook
    }


view : Int -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]