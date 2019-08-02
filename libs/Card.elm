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


view : Float -> ( Float, Float ) -> Card msg -> Element msg
view scale ( width, height ) { attributes, selected, onPress, content } =
    let
        att : List (Attribute msg)
        att =
            List.concat
                [ [ Element.width <| Element.px <| floor <| width * scale
                  , Element.height <| Element.px <| floor <| height * scale
                  , Border.width 1
                  , Border.color <| Element.rgba255 219 219 219 1
                  , Border.rounded <| floor <| 4 * scale
                  , Element.padding <| floor <| 5 * scale
                  , Background.color <| Element.rgb255 255 255 255
                  ]
                , attributes
                ]
    in
    if selected then
        Element.el
            (Element.alignTop :: att)
        <|
            content

    else
        Input.button
            ([ Element.mouseOver
                [ Border.color <| Element.rgb255 155 203 255
                ]
             , Element.alignBottom
             ]
                ++ att
            )
            { label = content
            , onPress = onPress
            }


hand : List (Attribute msg) -> { scale : Float, dimensions : ( Float, Float ), width : Float, cards : List (Card msg) } -> Element msg
hand attributes { scale, dimensions, width, cards } =
    let
        cardsAmount : Float
        cardsAmount =
            cards |> List.length |> toFloat

        ( cardWidth, _ ) =
            dimensions

        spacing =
            if cardsAmount - 1 <= 0 then
                0

            else
                (width - scale * cardsAmount * cardWidth)
                    / (cardsAmount - 1)
                    |> clamp -(cardWidth * scale / 5) (cardWidth * scale / 5)

        {- |> \x -> if x> ((cardWidth) * scale / 5) then
           ((cardWidth) * scale / 5)
           else x
        -}
    in
    cards
        |> List.map (view scale dimensions)
        |> Element.row
            ([ --Element.width <| Element.px <| round <| width
               --,
               Element.spacing <| round <| spacing
             , Element.centerX
             ]
                ++ attributes
            )
