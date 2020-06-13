module Main exposing (..)

import Browser
import Browser.Events
import Helper exposing (flattenOpt)
import Html exposing (Html, div, fieldset, h1, input, label, text)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List.Extra
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Messages exposing (..)
import Model exposing (..)
import Random
import Set
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
    , hasEatenInLastMove = False
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

        newSnake =
            case game.hasEatenInLastMove of
                True ->
                    Nonempty.append (Nonempty.fromElement newHead) game.snake

                False ->
                    game.snake
                        |> Nonempty.reverse
                        |> Nonempty.tail
                        |> List.reverse
                        |> Nonempty newHead
    in
    -- Mental image: take the last element of the list and move it one tile ahead of the snakes current head in the current direction"
    newSnake
        |> Nonempty.map (\pos -> adjustPositionToGameSize game pos)


evaluateGameTick : Game -> Time.Posix -> ( Model, Cmd Msg )
evaluateGameTick game posix =
    let
        move =
            evaluateMove game.moves game.currentDirection

        snake =
            calcNewSnake game move

        hasEaten =
            Nonempty.head snake == game.apple

        applesEaten =
            if hasEaten then
                game.applesEaten + 1

            else
                game.applesEaten

        newGame =
            { game | currentTime = posix, snake = snake, hasEatenInLastMove = hasEaten, applesEaten = applesEaten, moves = [], currentDirection = move }

        appleCmd =
            case hasEaten of
                True ->
                    calcNewAppleCmd newGame

                False ->
                    Cmd.none
    in
    ( RunningGame newGame, appleCmd )


calcNewAppleCmd : Game -> Cmd Msg
calcNewAppleCmd game =
    let
        gamefield =
            List.range 0 (game.cols - 1)
                |> List.concatMap
                    (\x ->
                        List.range 0 (game.rows - 1)
                            |> List.map (\y -> ( x, y ))
                    )
                |> Set.fromList

        snakeSet =
            game.snake
                |> Nonempty.toList
                |> List.map (\vec -> ( vec.x, vec.y ))
                |> Set.fromList

        tilesWithoutSnake =
            Set.diff gamefield snakeSet
                |> Set.toList
                |> List.map (\tup -> { x = Tuple.first tup, y = Tuple.second tup })
    in
    Random.int 0 (List.length tilesWithoutSnake - 1)
        |> Random.map (\idx -> List.Extra.getAt idx tilesWithoutSnake)
        |> Random.generate
            (\maybeVec ->
                case maybeVec of
                    Just vec ->
                        NewApple vec

                    Nothing ->
                        GameVictory
            )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        RunningGame game ->
            case msg of
                Tick posix ->
                    evaluateGameTick game posix

                ToggleDebug ->
                    ( RunningGame { game | isDebug = not game.isDebug }, Cmd.none )

                DirectionChange newDirection ->
                    ( RunningGame { game | moves = List.append game.moves [ newDirection ] }, Cmd.none )

                NoOp ->
                    ( RunningGame game, Cmd.none )

                GotTimeInfos _ ->
                    ( model, Cmd.none )

                NewApple appleLocation ->
                    ( RunningGame { game | apple = appleLocation }, Cmd.none )

                GameVictory ->
                    ( Victory game, Cmd.none )

        NotStarted ->
            case msg of
                GotTimeInfos ( currentTime, currentZone ) ->
                    ( RunningGame (initializeGame currentZone currentTime), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Victory game ->
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

                Victory game ->
                    ( Just game.applesEaten, game.isDebug )

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

        Victory _ ->
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
