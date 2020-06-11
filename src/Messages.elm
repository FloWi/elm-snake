module Messages exposing (..)

import Model exposing (Vector)
import Time


type Msg
    = Tick Time.Posix
    | GotTimeInfos ( Time.Zone, Time.Posix )
    | ToggleDebug
    | DirectionChange Vector
    | NoOp
