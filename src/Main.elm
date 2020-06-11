module Main exposing (..)

import Browser
import Browser.Events
import Helper exposing (flattenOpt)
import Html exposing (Html, div, fieldset, h1, input, label, text)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
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
    , moves = []
    , currentDirection = east
    , snake = Nonempty.fromElement { x = 2, y = 2 }
    , apple = { x = 16, y = 2 }
    , startTime = time
    , currentZone = zone
    , isDebug = True
    , currentTime = time
    , applesEaten = 0
    }


addVector : Vector -> Vector -> Vector
addVector v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y }


adjustPositionToGameSize : Game -> Vector -> Vector
adjustPositionToGameSize game { x, y } =
    { x = modBy game.cols x, y = modBy game.rows y }


calcNewSnake : Game -> Vector -> Nonempty Vector
calcNewSnake game direction =
    let
        newHead =
            addVector (Nonempty.head game.snake) direction
    in
    -- Mental image: take the last element of the list and move it one tile ahead of the snakes current head in the current direction"
    game.snake
        |> Nonempty.reverse
        |> Nonempty.tail
        |> List.reverse
        |> Nonempty newHead
        |> Nonempty.map (\pos -> adjustPositionToGameSize game pos)


evaluateGameTick : Game -> Time.Posix -> Game
evaluateGameTick game posix =
    let
        move =
            evaluateMove game.moves game.currentDirection

        snake =
            calcNewSnake game move
    in
    { game | currentTime = posix, snake = snake, moves = [], currentDirection = move }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        RunningGame game ->
            case msg of
                Tick posix ->
                    ( RunningGame (evaluateGameTick game posix), Cmd.none )

                ToggleDebug ->
                    ( RunningGame { game | isDebug = not game.isDebug }, Cmd.none )

                DirectionChange newDirection ->
                    ( RunningGame { game | moves = List.append game.moves [ newDirection ] }, Cmd.none )

                NoOp ->
                    ( RunningGame game, Cmd.none )

                GotTimeInfos _ ->
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
            Sub.batch
                [ Browser.Events.onKeyDown keyDecoder
                , Time.every
                    (toFloat (tickInterval game))
                    Tick
                ]

        NotStarted ->
            Sub.none


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map toDirection


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            DirectionChange west

        "a" ->
            DirectionChange west

        "ArrowRight" ->
            DirectionChange east

        "d" ->
            DirectionChange east

        "ArrowUp" ->
            DirectionChange north

        "w" ->
            DirectionChange north

        "ArrowDown" ->
            DirectionChange south

        "s" ->
            DirectionChange south

        _ ->
            NoOp



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
