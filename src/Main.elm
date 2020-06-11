module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, input, label, text)
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
    , currentTime = time
    , currentZone = zone
    , isDebug = True
    }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        RunningGame game ->
            case msg of
                Tick posix ->
                    ( RunningGame { game | currentTime = posix }, Cmd.none )

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
    div
        [ Html.Attributes.class "gameView"
        ]
        [ div [ Html.Attributes.class "header" ]
            [ h1 [] [ text "Elm Snake" ]
            ]
        , renderGame
            model
        , div [ Html.Attributes.class "sidebar" ]
            [ h1 [] [ text "Sidebar" ]
            ]
        , div [ Html.Attributes.class "footer" ]
            [ h1 [] [ text "Footer" ]
            ]
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        RunningGame _ ->
            Time.every 1000 Tick

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
