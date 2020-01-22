module FactoryCity.View.Settings exposing (view)

import Element exposing (Element)
import Element.Input as Input
import Framework.Card as Card
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
            , Input.text Input.simple
                { onChange = String.toInt >> Maybe.withDefault loopLength >> changedLoopLengthMsg
                , text = String.fromInt <| loopLength
                , placeholder = Nothing
                , label = Input.labelHidden "Loop Length"
                }
            ]
        ]
