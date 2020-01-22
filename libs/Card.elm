module Card exposing (card, hand)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input


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


view : Int -> ( Float, Float ) -> Card msg -> Element msg
view amount ( width, height ) { attributes, selected, onPress, content } =
    let
        att : List (Attribute msg)
        att =
            List.concat
                [ [ Element.width <| Element.px <| floor <| width
                  , Element.height <| Element.px <| floor <| height
                  , Border.width 1
                  , Border.color <| Element.rgba255 219 219 219 1
                  , Border.rounded <| floor <| 4
                  , Element.padding <| floor <| 5
                  , Background.color <| Element.rgb255 255 255 255
                  ]
                , attributes
                ]
    in
    Element.el
        [ Element.height <| Element.px <| floor <| height * 1.1
        , Element.width <|
            Element.px <|
                if selected then
                    floor <| width

                else if amount < 5 then
                    floor <| width

                else
                    (floor <| width) * 4 // amount
        ]
    <|
        Input.button
            ([ Element.mouseOver
                [ Border.color <| Element.rgb255 155 203 255
                ]
             ]
                ++ (if selected then
                        [ Element.alignTop ]

                    else
                        [ Element.alignBottom ]
                   )
                ++ att
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

        spacing =
            if cardsAmount - 1 <= 0 then
                0

            else
                (width - toFloat cardsAmount * cardWidth)
                    / (toFloat cardsAmount - 1)
                    |> clamp -(cardWidth / 5) (cardWidth / 5)
    in
    cards
        |> List.map (view cardsAmount dimensions)
        |> Element.wrappedRow
            ([ Element.width <| Element.shrink
             , Element.height <| Element.shrink
             , Element.spacing <| round <| spacing
             , Element.centerX
             ]
                ++ attributes
            )
