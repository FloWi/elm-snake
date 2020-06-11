module Model exposing (..)

---- MODEL ----

import List.Extra
import List.Nonempty as Nonempty exposing (Nonempty)
import Time


type Model
    = RunningGame Game
    | NotStarted


tickInterval : Game -> Int
tickInterval game =
    1000 - min (game.applesEaten * 5) 100


evaluateMove : List Vector -> Vector -> Vector
evaluateMove moves currentDirection =
    case moves of
        [] ->
            currentDirection

        xs ->
            xs
                |> List.Extra.last
                |> Maybe.andThen (validateMove currentDirection)
                |> Maybe.withDefault currentDirection


validateMove : Vector -> Vector -> Maybe Vector
validateMove currentDirection lastMove =
    let
        isValid =
            not (currentDirection.x + lastMove.x == 0) || not (currentDirection.y + lastMove.y == 0)
    in
    if isValid then
        Just lastMove

    else
        Nothing


type alias Game =
    { cols : Int
    , rows : Int
    , moves : List Vector
    , currentDirection : Vector
    , snake : Nonempty Vector
    , apple : Vector
    , startTime : Time.Posix
    , currentTime : Time.Posix
    , currentZone : Time.Zone
    , isDebug : Bool
    , applesEaten : Int
    }


moveToString : Vector -> String
moveToString v =
    -- elm has a compiler bug that prevents it from matching negative numbers so I go with a chain of if statements
    if v == north then
        "north"

    else if v == south then
        "south"

    else if v == west then
        "west"

    else if v == east then
        "east"

    else
        "unknown"


type alias Vector =
    { x : Int
    , y : Int
    }


north : Vector
north =
    { x = 0, y = -1 }


south : Vector
south =
    { x = 0, y = 1 }


east : Vector
east =
    { x = 1, y = 0 }


west : Vector
west =
    { x = -1, y = 0 }
