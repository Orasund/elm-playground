module HeroForge.View.Game exposing (view)

import Element exposing (Element)
import Element.Font as Font
import Framework.Color as Color
import Framework.Grid as Grid
import HeroForge.Data.Card as Card exposing (Card(..))
import HeroForge.Data.Deck as Deck
import HeroForge.Data.Game as Game exposing (Direction, Game)
import HeroForge.Data.Item as Item
import HeroForge.Data.Level as Level
import HeroForge.View.Card as Card
import HeroForge.View.Deck as Deck
import Set.Any as AnySet


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
        , game.currentLevel
            |> Level.toString
            |> Element.text
            |> Element.el
                ([ Font.alignRight
                 , Font.bold
                 , Element.centerX
                 ]
                    ++ (if game.wonLevels |> AnySet.member game.currentLevel then
                            [ Font.color <| Color.green ]

                        else
                            []
                       )
                )
        , Element.row Grid.compact
            [ Element.el [ Element.width <| Element.fill ] <| Element.none
            , game.deck |> Deck.view
            ]
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
        , "Inventory"
            |> Element.text
            |> Element.el
                ([ Font.alignRight
                 , Font.bold
                 , Element.centerX
                 ]
                    ++ (if game.wonLevels |> AnySet.member game.currentLevel then
                            [ Font.color <| Color.green ]

                        else
                            []
                       )
                )
        , game.items
            |> AnySet.toList
            |> List.map (Item.asSymbol >> Element.text)
            |> Element.row [ Element.spacing 5, Element.centerX ]
        ]
