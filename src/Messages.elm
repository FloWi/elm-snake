module Messages exposing (..)

import Time


type Msg
    = Tick Time.Posix
    | GotTimeInfos ( Time.Zone, Time.Posix )
    | ToggleDebug
