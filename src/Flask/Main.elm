module Flask.Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Flask.Data.Effect as Effect exposing (Effect(..))
import Flask.Data.Element as Elem
import Framework
import Framework.Card as Card
import Framework.Grid as Grid
import Html exposing (Html)


baseMult : Float
baseMult =
    5


cardHeight : Int
cardHeight =
    round <| 160 * baseMult


cardWidth : Int
cardWidth =
    round <| 100 * baseMult


spacingMult : Float
spacingMult =
    1 * baseMult


fontMult : Float
fontMult =
    3.6 * baseMult


type alias Model =
    ()


type Msg
    = Idle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Idle ->
            ( (), Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( ()
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


textField : { title : String, desc : String } -> Element msg
textField { title, desc } =
    case desc of
        "" ->
            Element.el
                [ Font.size <| round <| 2 * fontMult
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
                    [ Font.size <| round <| 2 * fontMult ]
                  <|
                    Element.text title
                , Element.paragraph
                    [ Element.width <| Element.fill
                    , Font.size <| round <| 1 * fontMult
                    , Font.alignRight
                    ]
                  <|
                    List.singleton <|
                        Element.text text
                ]


card :
    { name : String
    , cost : ( List Elem.Element, Int )
    , effects : List Effect
    , hasDesc : Bool
    , code : String
    , img : String
    }
    -> Element msg
card { name, cost, effects, hasDesc, code, img } =
    let
        effectsAmount : Int
        effectsAmount =
            effects |> List.length
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
        [ Element.el
            [ Element.padding <| round <| 2 * spacingMult
            , Font.size <| round <| 2 * fontMult
            , Element.width <| Element.fill
            ]
          <|
            Element.text <|
                case cost of
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
        , Element.el
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
        , effects
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
                            ]
                )
            |> Element.column [ Element.spacing <| round <| 1 * spacingMult ]
        , Element.row
            [ Font.size <| round <| 1 * fontMult
            , Element.spaceEvenly
            , Element.alignBottom
            , Element.width <| Element.fill
            , Element.padding <| round <| 2 * spacingMult
            ]
          <|
            [ Element.text code
            , Element.text "v0.0.1"
            ]
        ]


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [] <|
            [ Element.paragraph [ Element.width <| Element.px <| 4 * cardWidth ] <|
                [ card
                    { name = "Research"
                    , cost = ( [], 1 )
                    , effects = [ Add [ Elem.Blue, Elem.Blue ] ]
                    , hasDesc = True
                    , code = "B1"
                    , img = "research.jpg"
                    }
                , card
                    { name = "Invent"
                    , cost = ( [ Elem.Blue ], 0 )
                    , effects = [ Draw 1 ]
                    , hasDesc = True
                    , code = "B2"
                    , img = "invent.jpg"
                    }
                , card
                    { name = "Attack"
                    , cost = ( [ Elem.Red ], 0 )
                    , effects = [ Remove 2 ]
                    , hasDesc = True
                    , code = "R1"
                    , img = "attack.jpg"
                    }
                , card
                    { name = "Sabotage"
                    , cost = ( [ Elem.Red ], 0 )
                    , effects = [ Discard 2 ]
                    , hasDesc = True
                    , code = "R2"
                    , img = "sabotage.jpg"
                    }
                , card
                    { name = "Sale"
                    , cost = ( [ Elem.Red, Elem.Blue ], 0 )
                    , effects = [ Add [ Elem.Yellow, Elem.Yellow, Elem.Yellow ] ]
                    , hasDesc = True
                    , code = "Y1"
                    , img = "sale.jpg"
                    }
                , card
                    { name = "Reboot"
                    , cost = ( [ Elem.Yellow, Elem.Yellow ], 0 )
                    , effects = [ Reboot ]
                    , hasDesc = True
                    , code = "Y2"
                    , img = "reboot.jpg"
                    }
                , card
                    { name = "Exchange"
                    , cost = ( [ Elem.Yellow, Elem.Any ], 0 )
                    , effects = [ Choose ]
                    , hasDesc = True
                    , code = "G1"
                    , img = "exchange.jpg"
                    }
                , card
                    { name = "Plant"
                    , cost = ( [ Elem.Green, Elem.Green ], 0 )
                    , effects = [ Plant ]
                    , hasDesc = True
                    , code = "G2"
                    , img = "plant.jpg"
                    }
                ]
            , Element.text "Zum Drucken die Seitenskalierung auf 38% stellen bei 600dpi"
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
