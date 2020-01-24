module FactoryCity.View.Settings exposing (view)

import Element exposing (Element)
import Element.Input as Input
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Input as Input


view :
    { changedLoopLengthMsg : Int -> msg
    , loopLength : Int
    , hasPower : Bool
    , togglePowerMsg : msg
    }
    -> Element msg
view { changedLoopLengthMsg, loopLength, hasPower, togglePowerMsg } =
    Element.column Grid.section <|
        [ Element.row Grid.spaceEvenly <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text <|
                        "Loop Length"
            , [ 5, 10, 15, 20 ]
                |> List.indexedMap
                    (\i n ->
                        Input.button
                            ((if i == 0 then
                                Button.groupLeft

                              else if i == 3 then
                                Button.groupRight

                              else
                                Button.groupCenter
                             )
                                ++ (if loopLength == n then
                                        Color.primary

                                    else
                                        []
                                   )
                            )
                            { onPress = Just <| changedLoopLengthMsg <| n
                            , label = Element.text <| String.fromInt <| n
                            }
                    )
                |> Element.row Grid.compact
            ]
        , Element.row Grid.spaceEvenly <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text <|
                        "Power"
            , Input.button
                (Button.simple
                    ++ (if hasPower then
                            Color.primary

                        else
                            []
                       )
                )
                { onPress = Just <| togglePowerMsg
                , label =
                    Element.text <|
                        if hasPower then
                            "on"

                        else
                            "off"
                }
            ]
        ]
