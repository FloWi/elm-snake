module SvgRenderer exposing (..)

import Helper exposing (flattenMaybes)
import Html exposing (Html, div)
import Html.Attributes
import List.Nonempty as Nonempty exposing (Nonempty)
import Messages exposing (..)
import Model exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)
import Time exposing (toHour, toMinute, toSecond)


toTimeString : Time.Posix -> Time.Zone -> String
toTimeString time zone =
    let
        twoDigitNumber int =
            String.fromInt int
                |> String.padLeft 2 '0'
    in
    twoDigitNumber (toHour zone time)
        ++ ":"
        ++ twoDigitNumber (toMinute zone time)
        ++ ":"
        ++ twoDigitNumber (toSecond zone time)


renderGame : Model -> Svg msg
renderGame model =
    case model of
        RunningGame game ->
            let
                contentClass =
                    if game.isDebug then
                        "contentDebug"

                    else
                        "content"

                elapsedTime =
                    Time.posixToMillis game.currentTime - Time.posixToMillis game.startTime

                elapsedTimeMsString =
                    String.fromInt elapsedTime ++ "ms"

                debugScreenDiv =
                    if not game.isDebug then
                        Nothing

                    else
                        Just
                            (div [ Html.Attributes.class "debugScreen" ]
                                [ Html.pre []
                                    [ Html.text
                                        ("  start time: "
                                            ++ toTimeString game.startTime game.currentZone
                                            ++ "\ncurrent time: "
                                            ++ toTimeString game.currentTime game.currentZone
                                            ++ "\nelapsed time: "
                                            ++ elapsedTimeMsString
                                            ++ "\ntick interval: "
                                            ++ String.fromInt (tickInterval game)
                                            ++ "ms"
                                        )
                                    ]
                                ]
                            )

                snake =
                    drawListBlocks game.snake "snake" game

                apple =
                    drawListBlocks (Nonempty.fromElement game.apple) "apple" game

                gameScreenDiv =
                    div [ Html.Attributes.class "gameScreen" ]
                        [ svg
                            [ class "svgGame"
                            , preserveAspectRatio "xMinYMin meet"
                            , viewBox "0 0 700 500"
                            ]
                            [ snake
                            , apple
                            ]
                        ]

                divs =
                    flattenMaybes
                        [ Just gameScreenDiv
                        , debugScreenDiv
                        ]
            in
            div
                [ Html.Attributes.class contentClass ]
                divs

        NotStarted ->
            div []
                [ Html.h1 [] [ Html.text "Game hasn't been started yet" ]
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
