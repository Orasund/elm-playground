module FactoryCity.View.Settings exposing (view)

import Element exposing (Element)
import Element.Input as Input
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Input as Input


view : { changedLoopLengthMsg : Int -> msg, loopLength : Int } -> Element msg
view { changedLoopLengthMsg, loopLength } =
    Element.column Grid.section <|
        [ Element.row Grid.spaceEvenly <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text <|
                        "Loop Length"
            , [ 5, 8, 11, 14 ]
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
        ]
