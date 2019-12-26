module Flask.View.Card exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Flask.Data exposing (baseMult, cardHeight, cardWidth, fontMult, spacingMult)
import Flask.Data.Effect as Effect exposing (Effect(..))
import Flask.Data.Element as Elem


textField : { title : String, desc : String } -> Element msg
textField { title, desc } =
    case desc of
        "" ->
            Element.el
                [ Font.size <| round <| 2 * fontMult
                , Element.width Element.fill
                , Font.center
                ]
            <|
                Element.text title

        text ->
            Element.column
                [ Element.spacing <| round <| 1 * spacingMult
                , Element.width <| Element.fill
                ]
            <|
                [ Element.el
                    [ Font.size <| round <| 2 * fontMult, Element.centerX ]
                  <|
                    Element.text title
                , Element.paragraph
                    [ Element.width <| Element.fill
                    , Font.size <| round <| 1 * fontMult
                    , Font.alignLeft
                    ]
                  <|
                    List.singleton <|
                        Element.text text
                ]


view :
    { name : String
    , cost : ( List Elem.Element, Int )
    , effects : List Effect
    , hasDesc : Bool
    , code : String
    , img : String
    }
    -> Element msg
view { name, cost, effects, hasDesc, code, img } =
    let
        effectsAmount : Int
        effectsAmount =
            effects |> List.length

        viewEffects : Element msg
        viewEffects =
            if effectsAmount == 0 then
                Element.el [ Element.height <| Element.fill ] <| Element.none

            else
                effects
                    |> List.map
                        (\effect ->
                            effect
                                |> Effect.toTextField
                                |> (if
                                        hasDesc
                                            && (case effect of
                                                    Plant ->
                                                        effectsAmount <= 1

                                                    _ ->
                                                        effectsAmount <= 2
                                               )
                                    then
                                        identity

                                    else
                                        \field -> { field | desc = "" }
                                   )
                                |> textField
                                |> Element.el
                                    [ Font.size <| round <| 2 * fontMult
                                    , Element.padding <| round <| 2 * spacingMult
                                    , Element.width <| Element.fill
                                    ]
                        )
                    |> Element.column [ Element.spacing <| round <| 1 * spacingMult, Element.width <| Element.fill ]
    in
    Element.column
        [ Element.width <| Element.px <| cardWidth
        , Element.height <| Element.px <| cardHeight
        , Background.color <| Element.rgb255 255 255 255
        , Border.width <| round <| 2 * spacingMult
        , Border.solid
        , Border.color <| Element.rgb255 0 0 0
        , Element.spacing <| round <| 1 * spacingMult
        ]
    <|
        List.concat
            [ [ Element.el
                    [ Element.padding <| round <| 2 * spacingMult
                    , Font.size <| round <| 2 * fontMult
                    , Element.width <| Element.fill
                    ]
                <|
                    Element.text <|
                        case cost of
                            ( [], 0 ) ->
                                " "

                            ( [], n ) ->
                                String.fromInt n
                                    ++ " "
                                    ++ (if n <= 1 then
                                            "Card"

                                        else
                                            "Cards"
                                       )

                            ( list, 0 ) ->
                                list
                                    |> List.map Elem.toString
                                    |> String.concat

                            ( list, n ) ->
                                (list
                                    |> List.map Elem.toString
                                    |> String.concat
                                )
                                    ++ " and "
                                    ++ String.fromInt n
                                    ++ " "
                                    ++ (if n <= 1 then
                                            "Card"

                                        else
                                            "Cards"
                                       )
              , Element.el
                    [ Font.size <| round <| 2 * fontMult
                    , Element.width <| Element.fill
                    , Font.center
                    ]
                <|
                    Element.text name
              ]
            , [ Element.el
                    [ Element.paddingEach
                        { bottom = 0
                        , left = round <| 2 * spacingMult
                        , right = 0
                        , top = 0
                        }
                    , Element.height <| Element.fill
                    , Element.width <| Element.fill
                    ]
                <|
                    Element.el
                        [ Element.height <| Element.fill
                        , Element.width <| Element.fill
                        , Border.widthEach
                            { bottom = round <| 1 * spacingMult
                            , left = round <| 1 * spacingMult
                            , right = 0
                            , top = round <| 1 * spacingMult
                            }
                        , Border.roundEach
                            { topLeft = round <| 16 * baseMult
                            , topRight = 0
                            , bottomLeft = round <| 16 * baseMult
                            , bottomRight = 0
                            }
                        , Background.image img
                        ]
                    <|
                        Element.none
              , viewEffects
              ]
            , List.singleton <|
                Element.row
                    [ Font.size <| round <| 1 * fontMult
                    , Element.spaceEvenly
                    , Element.alignBottom
                    , Element.width <| Element.fill
                    , Element.padding <| round <| 2 * spacingMult
                    ]
                <|
                    [ Element.text code
                    , Element.text "v0.2.0"
                    ]
            ]
