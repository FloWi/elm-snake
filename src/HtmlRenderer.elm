module HtmlRenderer exposing (..)

import Html exposing (Html, div, h1, h2, p, text)
import Html.Attributes
import Messages exposing (..)
import Model exposing (..)


renderHtml : Model -> Html Msg
renderHtml model =
    case model of
        RunningGame game ->
            let
                debugScreenElements =
                    if game.isDebug then
                        [ renderGameSlots "Game Slots" game.gameSlots
                        , renderGameSlots "Draw new card stacks" game.drawCardSlots
                        , renderGameSlots "completed card slots" game.completedCardSlots
                        ]

                    else
                        []
            in
            div
                [ Html.Attributes.class
                    (if game.isDebug then
                        "contentDebug"

                     else
                        "content"
                    )
                ]
                [ div [ Html.Attributes.class "gameScreen" ]
                    [ renderAllCardsAsImages model
                    ]
                , div [ Html.Attributes.class "debugScreen" ]
                    debugScreenElements
                ]


renderCard : Card -> Html msg
renderCard card =
    let
        rank =
            rankString card.rank

        suit =
            suitUtf8Symbol card.suit

        hidden : List (Html.Attribute msg)
        hidden =
            if card.isFacedUp then
                []

            else
                [ Html.Attributes.class "hiddenCard" ]
    in
    p hidden [ text (suit ++ rank) ]


renderGameSlots : String -> List (List Card) -> Html msg
renderGameSlots label gameSlots =
    let
        renderGameSlot : List Card -> Html msg
        renderGameSlot cards =
            cards
                |> List.map (\card -> p [] [ renderCard card ])
                |> div []

        gameSlotDivs : List (Html msg)
        gameSlotDivs =
            gameSlots
                |> List.indexedMap Tuple.pair
                |> List.map
                    (\( i, cards ) ->
                        div
                            []
                            [ text (String.fromInt i)
                            , renderGameSlot cards
                            ]
                    )
    in
    div []
        [ h2 [] [ text label ]
        , div
            [ Html.Attributes.class "gameStacks" ]
            gameSlotDivs
        ]


renderAllCardsAsImages : Model -> Html Msg
renderAllCardsAsImages model =
    let
        deck =
            List.map (\card -> { card | isFacedUp = True }) calcDeck
                ++ [ { suit = Clubs, rank = Jack, isFacedUp = False } ]
    in
    div []
        [ h1 [] [ text "Game is on" ]
        , renderDeck deck
        ]


renderDeck : List Card -> Html Msg
renderDeck cards =
    let
        images =
            cards
                |> List.map
                    (\card ->
                        Html.img
                            [ Html.Attributes.src (cardImgUrl card)
                            , Html.Attributes.class "cardImage"
                            ]
                            []
                    )
    in
    div
        [ Html.Attributes.class "allCards" ]
        images
