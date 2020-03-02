module Card exposing (card, hand, view)

import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Color as Color


type alias Card msg =
    { attributes : List (Attribute msg)
    , selected : Bool
    , onPress : Maybe msg
    , content : Element msg
    }


card :
    { attributes : List (Attribute msg), selected : Bool, onPress : Maybe msg, content : Element msg }
    -> Card msg
card { attributes, selected, onPress, content } =
    { attributes = attributes
    , selected = selected
    , onPress = onPress
    , content = content
    }


view : { availableSpace : Float, amount : Int, dim : ( Float, Float ) } -> Card msg -> Element msg
view { availableSpace, amount, dim } { attributes, selected, onPress, content } =
    let
        ( width, height ) =
            dim
    in
    Element.el
        [ Element.height <| Element.px <| floor <| height * 1.1
        , Element.width <|
            Element.px <|
                if selected then
                    round <| width

                else if availableSpace / toFloat amount >= width then
                    round <| width

                else
                    -- used a geometric series: s - 2w + c / (a - 1 ) = c
                    (availableSpace - 2 * width)
                        / toFloat (amount - 2)
                        |> round
        ]
    <|
        Input.button
            (List.concat
                [ Button.simple
                , Color.light
                , if selected then
                    [ Element.alignTop ]

                  else
                    [ Element.alignBottom ]
                , [ Element.width <| Element.px <| floor <| width
                  , Element.height <| Element.px <| floor <| height
                  , Font.alignLeft
                  , Border.color <| Color.lightGrey
                  ]
                , attributes
                ]
            )
            { label = content
            , onPress = onPress
            }


hand : List (Attribute msg) -> { dimensions : ( Float, Float ), width : Float, cards : List (Card msg) } -> Element msg
hand attributes { dimensions, width, cards } =
    let
        cardsAmount : Int
        cardsAmount =
            cards |> List.length

        ( cardWidth, _ ) =
            dimensions
    in
    cards
        |> List.map
            (view
                { amount = cardsAmount
                , availableSpace = width
                , dim = dimensions
                }
            )
        |> Element.row
            ([ Element.width <| Element.px <| round <| width
             , Element.height <| Element.fill
             , Element.centerX
             ]
                ++ (if width / toFloat cardsAmount >= cardWidth then
                        [ Element.spaceEvenly
                        ]

                    else
                        []
                   )
                ++ attributes
            )
