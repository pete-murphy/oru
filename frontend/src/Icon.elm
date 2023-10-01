module Icon exposing (..)

import Html exposing (Html)
import Maybe exposing (Maybe(..))
import Svg
import Svg.Attributes


search : Html msg
search =
    Svg.svg
        [ Svg.Attributes.viewBox "0 0 20 20"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.class "h-5 w-5 fill-gray-500"
        ]
        [ Svg.path
            [ Svg.Attributes.fillRule "evenodd"
            , Svg.Attributes.d "M9 3.5a5.5 5.5 0 100 11 5.5 5.5 0 000-11zM2 9a7 7 0 1112.452 4.391l3.328 3.329a.75.75 0 11-1.06 1.06l-3.329-3.328A7 7 0 012 9z"
            , Svg.Attributes.clipRule "evenodd"
            ]
            []
        ]
