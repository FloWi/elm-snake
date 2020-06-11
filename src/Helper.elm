module Helper exposing (flattenMaybes)


flattenMaybes : List (Maybe a) -> List a
flattenMaybes maybes =
    let
        map a =
            case a of
                Just x ->
                    [ x ]

                Nothing ->
                    []
    in
    maybes
        |> List.concatMap map
