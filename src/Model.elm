module Model exposing (..)

---- MODEL ----


type Move
    = DrawNewCards



--| MoveCardStack { startCardIndex : CardIndex, cardStack : CardStack, destinationSlotIndex : Int }


type Model
    = RunningGame Game


type alias Game =
    { cols : Int
    , rows : Int
    , moves : List Vector
    , snake : List Vector
    , apple : Vector
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
