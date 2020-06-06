module SvgRenderer exposing (..)

import Html exposing (Html, div)
import Html.Attributes
import Messages exposing (..)
import Model exposing (..)
import Svg exposing (svg)
import Svg.Attributes exposing (..)


renderGame model =
    case model of
        RunningGame _ ->
            let
                debugScreenDiv =
                    div [] []
            in
            div
                []
                [ div [ Html.Attributes.class "gameScreen" ]
                    [ svg
                        [ class "svgGame"
                        , preserveAspectRatio "xMinYMin meet"
                        , viewBox "0 0 1000 1000"
                        ]
                        []
                    ]
                , debugScreenDiv
                ]


renderDebugScreen : Game -> Html Msg
renderDebugScreen game =
    div [ Html.Attributes.class "debugScreen" ]
        [ Html.h2 []
            [ Html.text "DebugScreen" ]
        ]
