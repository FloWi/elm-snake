module Helper exposing (flattenOpt)


flattenOpt : List (Maybe a) -> List a
flattenOpt maybes =
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
