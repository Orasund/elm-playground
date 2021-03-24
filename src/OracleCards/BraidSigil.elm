module OracleCards.BraidSigil exposing (..)

import Browser
import Circle2d
import Element
import Geometry.Svg as Svg
import Html exposing (Html)
import OracleCards.Data.Alphabet as Alphabet exposing (TwentySix)
import OracleCards.View.BraidSigil as BraidSigil
import Pixels
import Point2d
import StaticArray
import StaticArray.Index exposing (Index)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Widget
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography



--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------


zoom : number
zoom =
    4


radius : number
radius =
    60


size : number
size =
    radius * 3


width : number
width =
    size


height : number
height =
    size


withCircle : Bool
withCircle =
    True


isGerman : Bool
isGerman =
    False


interactive : Bool
interactive =
    False


debugMode : Bool
debugMode =
    True



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    String


init : () -> ( Model, Cmd Msg )
init () =
    ( "", Cmd.none )



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = ChangedText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedText string ->
            ( string, Cmd.none )



--------------------------------------------------------------------------------
-- Subscriptions
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


asAlphabet : Char -> Index TwentySix
asAlphabet =
    if isGerman then
        Alphabet.german

    else
        Alphabet.english


viewSigil : String -> Html msg
viewSigil string =
    let
        options =
            { width = width
            , height = height
            , radius = radius
            }
    in
    [ case
        string
            |> String.toList
            |> List.map asAlphabet
      of
        head :: nextIndex :: tail ->
            nextIndex
                :: tail
                |> List.foldl
                    (\i2 ( state, out ) ->
                        let
                            ( newState, newOut ) =
                                BraidSigil.line options
                                    state
                                    i2
                        in
                        ( newState
                        , newOut :: out
                        )
                    )
                    (let
                        ( newState, newOut ) =
                            BraidSigil.line options
                                (BraidSigil.init options
                                    head
                                    nextIndex
                                )
                                head
                     in
                     ( newState
                     , newOut
                        :: []
                       --[ BraidSigil.initCircle options head nextIndex ]
                     )
                    )
                |> (\( state, out ) ->
                        let
                            ( newState, newOut ) =
                                BraidSigil.line options
                                    state
                                    head
                        in
                        ( newState
                        , newOut :: out
                        )
                   )
                |> (\( state, out ) ->
                        (BraidSigil.line options
                            { state
                                | visited = state.visited |> StaticArray.set head -1
                            }
                            head
                            |> Tuple.second
                        )
                            :: BraidSigil.initCircle options head nextIndex
                            :: out
                   )
                |> List.concat
                |> (if withCircle then
                        List.append
                            (Circle2d.atPoint (Point2d.pixels (size / 2) (size / 2))
                                (Pixels.pixels <| radius)
                                |> Svg.circle2d
                                    [ Attributes.fill <| "none"
                                    , Attributes.stroke <| "black"
                                    , Attributes.strokeWidth <| String.fromFloat <| BraidSigil.strokeWidth
                                    ]
                                |> List.singleton
                            )

                    else
                        identity
                   )

        _ ->
            []
    ]
        |> List.concat
        |> List.append
            (if debugMode then
                BraidSigil.drawPoints options

             else
                []
            )
        |> Svg.svg
            [ Attributes.width <| (String.fromFloat <| zoom * size) ++ "px"
            , Attributes.height <| (String.fromFloat <| zoom * size) ++ "px"
            , Attributes.version <| "1.1"
            , Attributes.viewBox <|
                "0 0 "
                    ++ String.fromFloat size
                    ++ " "
                    ++ String.fromFloat size
            ]


view : Model -> Html Msg
view model =
    if interactive then
        [ "Sigil Generator"
            |> Element.text
            |> Element.el Typography.h6
            |> Widget.asItem
        , Widget.fullBleedItem (Material.fullBleedItem Material.defaultPalette)
            { text = "Write a single word into the input field."
            , onPress = Nothing
            , icon = always Element.none
            }
        , Widget.textInput (Material.textInput Material.defaultPalette)
            { chips = []
            , text = model
            , placeholder = Nothing
            , label = "Word"
            , onChange = ChangedText
            }
            |> Widget.asItem
        , Widget.divider (Material.fullBleedDivider Material.defaultPalette)
        , viewSigil model
            |> Element.html
            |> Element.el [ Element.centerX ]
            |> Widget.asItem
        ]
            |> Widget.itemList
                (Material.cardColumn Material.defaultPalette
                    |> Customize.elementColumn
                        [ Element.width <| Element.px <| zoom * size + 32
                        , Element.centerX
                        , Element.centerY
                        ]
                    |> Customize.mapContent
                        (Customize.element
                            [ Element.width <| Element.px <| zoom * size + 32
                            ]
                        )
                )
            |> Element.layout []

    else
        let
            paths =
                --[ "Glueck", "Liebe", "Geld", "Erfolg" ]
                --[ "Luck", "Love", "Money", "Success" ]
                [ "Gegenwart"
                , "Ziel"
                , "Kraft"
                , "Bedeutung"
                , "Dankbarkeit"
                , "Werte"
                , "Gedanken"
                , "Zukunft"
                ]

            {--[ "eee"
                , "ebebebebe"
                , "aeiaei"
                , "aeiou"
                ]--}
        in
        paths
            |> List.map viewSigil
            |> Html.div []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
