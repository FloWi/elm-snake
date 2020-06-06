module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, input, label, p, text)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onClick)
import Messages exposing (..)
import Model exposing (..)
import Random exposing (..)
import SvgRenderer exposing (..)


init : ( Model, Cmd Msg )
init =
    ( Model.RunningGame initialGame
    , Cmd.none
    )


initialGame : Game
initialGame =
    { cols = 20
    , rows = 14
    , moves = [ east ]
    , snake = []
    , apple = { x = 16, y = 2 }
    }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
            , p [] [ text "inspired by Christopher Okhravi's excellent ", Html.a [ Html.Attributes.href "https://www.youtube.com/playlist?list=PLrhzvIcii6GOfRvsaVVdYSRjRhPWgAIKc" ] [ text "Game Development with Functional Programming in JavaScript series" ] ]
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
