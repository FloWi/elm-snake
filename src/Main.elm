module Main exposing (..)

import Browser
import Helper exposing (flattenOpt)
import Html exposing (Html, div, fieldset, h1, input, label, text)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onClick)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Messages exposing (..)
import Model exposing (..)
import SvgRenderer exposing (..)
import Task
import Time exposing (Posix, Zone)


init : ( Model, Cmd Msg )
init =
    ( Model.NotStarted
    , Task.perform GotTimeInfos (Task.map2 Tuple.pair Time.here Time.now)
    )


initializeGame : Time.Posix -> Time.Zone -> Game
initializeGame time zone =
    { cols = 20
    , rows = 14
    , moves = Nonempty.fromElement east
    , snake = Nonempty.fromElement { x = 2, y = 2 }
    , apple = { x = 16, y = 2 }
    , startTime = time
    , currentZone = zone
    , isDebug = False
    , currentTime = time
    , applesEaten = 0
    }


addVector : Vector -> Vector -> Vector
addVector v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y }


applyMove : Game -> Nonempty Vector
applyMove game =
    let
        move =
            calcMove game
    in
    calcNewSnake move game.snake


calcMove : Game -> Vector
calcMove game =
    Nonempty.head game.moves


calcNewSnake : Vector -> Nonempty Vector -> Nonempty Vector
calcNewSnake direction snake =
    let
        newHead =
            addVector (Nonempty.head snake) direction
    in
    snake
        |> Nonempty.reverse
        |> Nonempty.tail
        |> List.reverse
        |> Nonempty newHead



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        RunningGame game ->
            case msg of
                Tick posix ->
                    ( RunningGame { game | currentTime = posix, snake = applyMove game }, Cmd.none )

                ToggleDebug ->
                    ( RunningGame { game | isDebug = not game.isDebug }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NotStarted ->
            case msg of
                GotTimeInfos ( currentTime, currentZone ) ->
                    ( RunningGame (initializeGame currentZone currentTime), Cmd.none )

                _ ->
                    ( model, Cmd.none )



---- VIEW ----


checkbox : Msg -> String -> Bool -> Html Msg
checkbox msg name isChecked =
    label
        [ style "padding" "20px" ]
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text name
        ]


view : Model -> Html Msg
view model =
    let
        ( level, isDebug ) =
            case model of
                RunningGame game ->
                    ( Just game.applesEaten, game.isDebug )

                NotStarted ->
                    ( Nothing, False )

        levelDiv =
            level
                |> Maybe.map (\l -> div [] [ text ("Level " ++ String.fromInt l) ])

        sideBarElements =
            flattenOpt
                [ Just (h1 [] [ text "Sidebar" ])
                , Just
                    (fieldset []
                        [ checkbox ToggleDebug "debugView" isDebug ]
                    )
                , levelDiv
                ]
    in
    div
        [ Html.Attributes.class "gameView"
        ]
        [ div [ Html.Attributes.class "header" ]
            [ h1 [] [ text "Elm Snake" ]
            ]
        , renderGame
            model
        , div [ Html.Attributes.class "sidebar" ]
            sideBarElements
        , div [ Html.Attributes.class "footer" ]
            [ h1 [] [ text "Footer" ]
            ]
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        RunningGame game ->
            Time.every
                (toFloat (tickInterval game))
                Tick

        NotStarted ->
            Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
