module Model exposing (..)

---- MODEL ----
--| MoveCardStack { startCardIndex : CardIndex, cardStack : CardStack, destinationSlotIndex : Int }

import List.Nonempty as Nonempty exposing (Nonempty)
import Time


type Model
    = RunningGame Game
    | NotStarted


tickInterval : Game -> Int
tickInterval game =
    1000 - min (game.applesEaten * 5) 100


nextHead : Game -> Vector
nextHead game =
    let
        snakeHead =
            Nonempty.head game.snake

        movesHead =
            Nonempty.head game.moves
    in
    { x = modBy game.cols (snakeHead.x + movesHead.x)
    , y = modBy game.rows (snakeHead.y + movesHead.y)
    }


type alias Game =
    { cols : Int
    , rows : Int
    , moves : Nonempty Vector
    , snake : Nonempty Vector
    , apple : Vector
    , startTime : Time.Posix
    , currentTime : Time.Posix
    , currentZone : Time.Zone
    , isDebug : Bool
    , applesEaten : Int
    }


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
