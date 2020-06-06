module SvgRenderer exposing (..)

import Html exposing (Html, div)
import Html.Attributes
import List.Nonempty as Nonempty exposing (Nonempty)
import Messages exposing (..)
import Model exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


renderGame : Model -> Svg msg
renderGame model =
    case model of
        RunningGame game ->
            let
                debugScreenDiv =
                    div [] []

                snake =
                    drawListBlocks game.snake "snake" game

                apple =
                    drawListBlocks (Nonempty.fromElement game.apple) "apple" game
            in
            div
                []
                [ div [ Html.Attributes.class "gameScreen" ]
                    [ svg
                        [ class "svgGame"
                        , preserveAspectRatio "xMinYMin meet"
                        , viewBox "0 0 700 500"
                        ]
                        [ snake
                        , apple
                        ]
                    ]
                , debugScreenDiv
                ]


drawListBlocks : Nonempty Vector -> String -> Game -> Svg msg
drawListBlocks coords cssClass game =
    coords
        |> Nonempty.toList
        |> List.map (drawRect cssClass game)
        |> Svg.g []


drawRect : String -> Game -> Vector -> Svg msg
drawRect class game { x, y } =
    let
        rectHeight =
            700 // game.cols

        rectWidth =
            500 // game.rows
    in
    Svg.rect
        [ Svg.Attributes.x (String.fromInt (x * rectWidth))
        , Svg.Attributes.y (String.fromInt (y * rectHeight))
        , Svg.Attributes.width (String.fromInt rectWidth)
        , Svg.Attributes.height (String.fromInt rectHeight)
        , Svg.Attributes.class class
        ]
        []


renderDebugScreen : Game -> Html Msg
renderDebugScreen game =
    div [ Html.Attributes.class "debugScreen" ]
        [ Html.h2 []
            [ Html.text "DebugScreen" ]
        ]
