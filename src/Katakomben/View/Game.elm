module Katakomben.View.Game exposing (view)

import Element exposing (Element)
import Element.Font as Font
import Framework.Grid as Grid
import Katakomben.Data.Card as Card exposing (Card(..))
import Katakomben.Data.Deck as Deck
import Katakomben.Data.Game as Game exposing (Direction, Game)
import Katakomben.View.Card as Card
import Katakomben.View.Deck as Deck


bold : String -> Element msg
bold =
    Element.text >> Element.el [ Font.bold ]


view :
    { selected : Maybe Direction
    , showAnimation : Bool
    }
    -> Game
    -> Element Card.Msg
view { selected, showAnimation } game =
    Element.column Grid.simple
        [ Element.wrappedRow Grid.spaceEvenly
            [ Element.text <|
                "Health:"
                    ++ String.fromInt game.health
                    ++ "/"
                    ++ String.fromInt game.maxHealth
            , Element.text <|
                "Money:"
                    ++ String.fromInt game.money
            , Element.text <|
                "Attack:"
                    ++ String.fromInt game.attack
            ]
        , game.deck |> Deck.view
        , Card.view
            { selected = selected
            , card = game |> Game.current
            , maybeNextCard = game.deck |> Deck.next |> Deck.current
            , showAnimation = showAnimation
            }
        , [ "Use your " |> Element.text
          , "left" |> bold
          , " and " |> Element.text
          , "right" |> bold
          , " arrow keys to play the game." |> Element.text
          ]
            |> Element.paragraph
                [ Font.italic
                , Element.centerX
                , Element.width <| Element.shrink
                ]
        ]
